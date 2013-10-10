DROP PROCEDURE IF EXISTS adjacency_list_to_nested_set //

CREATE PROCEDURE adjacency_list_to_nested_set(
	src_table  VARCHAR(64),
	src_child  VARCHAR(64),
	src_parent VARCHAR(64),
	dst_table  VARCHAR(64),
	dst_node   VARCHAR(64),
	dst_left   VARCHAR(64),
	dst_right  VARCHAR(64),
	dst_level  VARCHAR(64)
)
BEGIN
	DECLARE max_right_edge INTEGER DEFAULT 0;
	DECLARE rows           INTEGER DEFAULT 0;
	DECLARE current        INTEGER DEFAULT 1;
	DECLARE trees          INTEGER DEFAULT 1;
	DECLARE next_edge      INTEGER DEFAULT 2;

	-- Temporary table to hold source data
	CREATE TEMPORARY TABLE IF NOT EXISTS _working_tree (
		child_id  INTEGER,
		parent_id INTEGER,
		PRIMARY KEY     (child_id),
		        KEY ix1 (parent_id, child_id)
	) ENGINE=InnoDB;
	TRUNCATE TABLE _working_tree;

	-- Temporary table to hold destination data
	CREATE TEMPORARY TABLE IF NOT EXISTS _nested_set (
		node_id    INTEGER,
		left_edge  INTEGER,
		right_edge INTEGER,
		level      INTEGER,
		KEY ix1 (node_id, left_edge, right_edge),
		KEY ix2 (level, node_id)
	) ENGINE=InnoDB;
	TRUNCATE TABLE _nested_set;

	-- Dynamic SQL, to interact with the source and destination tables
	SET @sql_read_src := CONCAT(
		'INSERT INTO _working_tree (child_id, parent_id )',
		' SELECT ', src_child, ', ', src_parent,
		' FROM ', src_table
	);
	PREPARE stmt_read_src FROM @sql_read_src;

	SET @sql_delete_dst := CONCAT(
		'DELETE FROM ', dst_table
	);
	PREPARE stmt_delete_dst FROM @sql_delete_dst;
	
	SET @sql_write_dst := CONCAT(
		'INSERT INTO ', dst_table, '(', dst_node, ', ', dst_left, ', ', dst_right, ', ', dst_level, ')',
		' SELECT node_id, left_edge, right_edge, ABS(level)',
		' FROM _nested_set'
	);
	PREPARE stmt_write_dst FROM @sql_write_dst;

	-- Read data from the source table
 	EXECUTE stmt_read_src;

	 -- root is child with NULL parent or parent which is not a child
  SELECT COUNT(*) INTO @nulls FROM _working_tree WHERE parent_id IS NULL;

	IF @nulls>1 THEN
		SET trees := 2;
  ELSEIF @nulls=1 THEN
    SELECT child_id INTO @root FROM _working_tree WHERE parent_id IS NULL;
    DELETE FROM _working_tree WHERE child_id=@root;
  ELSE
    SET @sql := CONCAT(
			'SELECT COUNT(DISTINCT f.', src_parent, ') INTO @roots',
			' FROM ', src_table, ' f',
			' LEFT JOIN ', src_table, ' t ON f.', src_parent, '=', 't.', src_child,
			' WHERE t.', src_child, ' IS NULL'
		);
    PREPARE stmt FROM @sql;
		EXECUTE stmt;
		DROP PREPARE stmt;

    IF @roots <> 1 THEN
			SET trees := @roots;
    ELSE
      SET @sql := CONCAT(
				'SELECT DISTINCT f.', src_parent, ' INTO @root',
				' FROM ', src_table, ' f',
				' LEFT JOIN ', src_table, ' t ON f.', src_parent, '=', 't.', src_child,
				' WHERE t.', src_child, ' IS NULL'
			);
      PREPARE stmt FROM @sql;
			EXECUTE stmt;
			DROP PREPARE stmt;
    END IF;
  END IF;

	IF trees = 0 THEN
		SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'No tree found';
	END IF;
	IF trees > 1 THEN
		SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Multiple trees found';
	END IF;

	-- Calculate the far-right node
	SELECT 2 * (1 + (COUNT(*))) FROM _working_tree INTO max_right_edge;

	INSERT INTO _nested_set VALUES(@root, 1, max_right_edge, 1);

	SET @start_time := NOW();
	WHILE next_edge < max_right_edge DO -- Work from left to right
		-- Look for a child
		SELECT MIN(child_id) INTO @tmp
		FROM   _nested_set
		JOIN   _working_tree ON node_id = parent_id
		WHERE  level = current;

		IF @tmp IS NOT NULL THEN
			-- Moving down to a child - fill in the left edge
			INSERT INTO _nested_set (level, node_id, left_edge)
			VALUES (current + 1, @tmp, next_edge);

			-- Original code...
			-- DELETE _working_tree FROM _working_tree JOIN _nested_set ON node_id = child_id WHERE level = current + 1;
			DELETE FROM _working_tree WHERE child_id = @tmp;

			SET current := current  + 1;
		ELSE
			-- Moving up to a parent - fill in the right edge
			UPDATE _nested_set
			SET right_edge = next_edge, level = -level
			WHERE level = current;

			SET current := current  - 1;
		END IF;
		SET next_edge := next_edge + 1;

		-- Provide feedback
		IF MOD(next_edge, 10000) = 0 THEN
			SELECT next_edge, TIMEDIFF(NOW(), @start_time) AS seconds;
		END IF;
	END WHILE;

	-- Orphaned records?  Possibly the data was not hierarchical?
	IF EXISTS(SELECT 1 FROM _working_tree) THEN
		SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Orphaned records remain';
	END IF;

	-- Transfer data to the destination table
	EXECUTE stmt_delete_dst;
	EXECUTE stmt_write_dst;

	-- Clean up
	DROP PREPARE stmt_read_src;
	DROP PREPARE stmt_delete_dst;
	DROP PREPARE stmt_write_dst;
	DROP TEMPORARY TABLE _working_tree, _nested_set;
END //

