-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DELIMITER //

SET SESSION
	autocommit               := TRUE,
	character_set_client     := utf8mb4,
	character_set_results    := utf8mb4,
	character_set_connection := utf8mb4,
	collation_connection     := utf8mb4_unicode_ci,
	foreign_key_checks       := TRUE,
	innodb_strict_mode       := ON,
	sql_mode                 := 'TRADITIONAL,NO_AUTO_VALUE_ON_ZERO',
	sql_notes                := TRUE,
	sql_warnings             := TRUE,
	unique_checks            := TRUE //

DROP DATABASE IF EXISTS fab_utils //
CREATE DATABASE fab_utils //
USE fab_utils //

ALTER DATABASE COLLATE utf8mb4_unicode_ci //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/latlon_to_osgb //

CREATE FUNCTION /*##*/latlon_to_osgb(
	p_latlon POINT
) RETURNS CHAR(12) CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert a WGS84 latitude/longitude to an OSGB grid reference'
	DETERMINISTIC
BEGIN
	DECLARE lat     DOUBLE DEFAULT RADIANS(Y(p_latlon));
	DECLARE lon     DOUBLE DEFAULT RADIANS(X(p_latlon));
	DECLARE a       DOUBLE DEFAULT 6377563.396;     
	DECLARE b       DOUBLE DEFAULT 6356256.910;     
	DECLARE F0      DOUBLE DEFAULT 0.9996012717;    
	DECLARE lat0    DOUBLE DEFAULT RADIANS(49.0);   
	DECLARE lon0    DOUBLE DEFAULT RADIANS(-2.0);   
	DECLARE N0      DOUBLE DEFAULT -100000.0;       
	DECLARE E0      DOUBLE DEFAULT  400000.0;       
	DECLARE e2      DOUBLE DEFAULT 1.0-(b*b)/(a*a); 
	DECLARE n       DOUBLE DEFAULT (a-b)/(a+b);
	DECLARE n2      DOUBLE DEFAULT n*n;
	DECLARE n3      DOUBLE DEFAULT n*n*n;
	DECLARE cosLat  DOUBLE DEFAULT COS(lat);
	DECLARE sinLat  DOUBLE DEFAULT SIN(lat);
  DECLARE nu      DOUBLE DEFAULT a*F0/SQRT(1-e2*sinLat*sinLat);            
  DECLARE rho     DOUBLE DEFAULT a*F0*(1-e2)/POW(1-e2*sinLat*sinLat, 1.5); 
  DECLARE eta2    DOUBLE DEFAULT nu/rho-1;
  DECLARE Ma      DOUBLE DEFAULT (1+n+(5/4)*n2+(5/4)*n3)*(lat-lat0);
  DECLARE Mb      DOUBLE DEFAULT (3*n+3*n*n+(21/8)*n3)*SIN(lat-lat0)*COS(lat+lat0);
  DECLARE Mc      DOUBLE DEFAULT ((15/8)*n2+(15/8)*n3)*SIN(2*(lat-lat0))*COS(2*(lat+lat0));
  DECLARE Md      DOUBLE DEFAULT (35/24)*n3*SIN(3*(lat-lat0))*COS(3*(lat+lat0));
  DECLARE M       DOUBLE DEFAULT b*F0*(Ma-Mb+Mc-Md); 
  DECLARE cos3lat DOUBLE DEFAULT cosLat*cosLat*cosLat;
  DECLARE cos5lat DOUBLE DEFAULT cos3lat*cosLat*cosLat;
  DECLARE tan2lat DOUBLE DEFAULT TAN(lat)*TAN(lat);
  DECLARE tan4lat DOUBLE DEFAULT tan2lat*tan2lat;
  DECLARE I       DOUBLE DEFAULT M + N0;
  DECLARE II      DOUBLE DEFAULT (nu/2)*sinLat*cosLat;
  DECLARE III     DOUBLE DEFAULT (nu/24)*sinLat*cos3lat*(5-tan2lat+9*eta2);
  DECLARE IIIA    DOUBLE DEFAULT (nu/720)*sinLat*cos5lat*(61-58*tan2lat+tan4lat);
  DECLARE IV      DOUBLE DEFAULT nu*cosLat;
  DECLARE V       DOUBLE DEFAULT (nu/6)*cos3lat*(nu/rho-tan2lat);
  DECLARE VI      DOUBLE DEFAULT (nu/120)*cos5lat*(5-18*tan2lat+tan4lat+14*eta2-58*tan2lat*eta2);
  DECLARE dLon    DOUBLE DEFAULT lon-lon0;
  DECLARE dLon2   DOUBLE DEFAULT dLon*dLon;
	DECLARE dLon3   DOUBLE DEFAULT dLon2*dLon;
	DECLARE dLon4   DOUBLE DEFAULT dLon3*dLon;
	DECLARE dLon5   DOUBLE DEFAULT dLon4*dLon;
	DECLARE dLon6   DOUBLE DEFAULT dLon5*dLon;

  DECLARE NORTHING INTEGER DEFAULT I+II*dLon2+III*dLon4+IIIA*dLon6;
  DECLARE EASTING  INTEGER DEFAULT E0+IV*dLon+V*dLon3+VI*dLon5;

	 RETURN CONCAT(
		CASE TRUNCATE(NORTHING/500000, 0)
		WHEN 0 THEN CASE TRUNCATE(EASTING/500000, 0) WHEN 0 THEN 'S' WHEN 1 THEN 'T' END
		WHEN 1 THEN CASE TRUNCATE(EASTING/500000, 0) WHEN 0 THEN 'N' WHEN 1 THEN 'O' END
		WHEN 2 THEN CASE TRUNCATE(EASTING/500000, 0) WHEN 0 THEN 'H' WHEN 1 THEN 'J' END
		END,
		CASE TRUNCATE((NORTHING%500000)/100000, 0)
		WHEN 0 THEN CASE TRUNCATE((EASTING%500000)/100000, 0) WHEN 0 THEN 'V' WHEN 1 THEN 'W' WHEN 2 THEN 'X' WHEN 3 THEN 'Y' WHEN 4 THEN 'Z' END
		WHEN 1 THEN CASE TRUNCATE((EASTING%500000)/100000, 0) WHEN 0 THEN 'Q' WHEN 1 THEN 'R' WHEN 2 THEN 'S' WHEN 3 THEN 'T' WHEN 4 THEN 'U' END
		WHEN 2 THEN CASE TRUNCATE((EASTING%500000)/100000, 0) WHEN 0 THEN 'L' WHEN 1 THEN 'M' WHEN 2 THEN 'N' WHEN 3 THEN 'O' WHEN 4 THEN 'P' END
		WHEN 3 THEN CASE TRUNCATE((EASTING%500000)/100000, 0) WHEN 0 THEN 'F' WHEN 1 THEN 'G' WHEN 2 THEN 'H' WHEN 3 THEN 'J' WHEN 4 THEN 'K' END
		WHEN 4 THEN CASE TRUNCATE((EASTING%500000)/100000, 0) WHEN 0 THEN 'A' WHEN 1 THEN 'B' WHEN 2 THEN 'C' WHEN 3 THEN 'D' WHEN 4 THEN 'E' END
		END,
		LPAD(EASTING %100000, 5, '0'),
		LPAD(NORTHING%100000, 5, '0')
	);
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/point_to_kml //

CREATE FUNCTION /*##*/point_to_kml(
	p_point POINT
) RETURNS TEXT CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert an OpenGIS point into an OpenGIS geometry a KML geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	RETURN CONCAT(
		'<Point><coordinates>', ROUND(X(p_point), 5), ',', ROUND(Y(p_point), 5), '</coordinates></Point>'
	);
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/polygon_to_kml //

CREATE FUNCTION /*##*/polygon_to_kml(
	p_polygon POLYGON
) RETURNS TEXT CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert an OpenGIS polygon into an OpenGIS geometry a KML geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	DECLARE kml TEXT DEFAULT '';
	DECLARE n INTEGER DEFAULT NUMINTERIORRINGS(p_polygon);

	WHILE n>0 DO
		SET kml := CONCAT(
			'<innerBoundaryIs>',
			/*##*/linestring_to_kml(INTERIORRINGN(p_polygon, n)),
			'</innerBoundaryIs>',
			kml
		);
		SET n := n-1;
	END WHILE;

	RETURN CONCAT('<Polygon><outerBoundaryIs>', /*##*/linestring_to_kml(EXTERIORRING(p_polygon)), '</outerBoundaryIs>',  kml, '</Polygon>');
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/osgb_to_latlon //

CREATE FUNCTION /*##*/osgb_to_latlon(
	p_point VARCHAR(16) CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
) RETURNS POINT
	COMMENT 'Convert an OSGB grid reference to a WGS84 latitude/longitude'
	DETERMINISTIC
BEGIN
	DECLARE EASTING DOUBLE DEFAULT
		CASE
		WHEN LEFT(p_gridref, 1) IN ('S', 'N', 'H') THEN      0.0
		WHEN LEFT(p_gridref, 1) IN ('T', 'O', 'J') THEN 500000.0
		END +
		CASE
		WHEN SUBSTR(p_gridref, 2, 1) IN ('A', 'F', 'L', 'Q', 'V') THEN      0.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('B', 'G', 'M', 'R', 'W') THEN 100000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('C', 'H', 'N', 'S', 'X') THEN 200000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('D', 'J', 'O', 'T', 'Y') THEN 300000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('E', 'K', 'P', 'U', 'Z') THEN 400000.0
		END +
		CAST(LEFT(CONCAT(SUBSTR(p_gridref, 3, LENGTH(p_gridref)/2-1), '50000'), 5) AS SIGNED INTEGER);
	DECLARE NORTHING DOUBLE DEFAULT
		CASE
		WHEN LEFT(p_gridref, 1) IN ('S', 'T') THEN       0.0
		WHEN LEFT(p_gridref, 1) IN ('N', 'O') THEN  500000.0
		WHEN LEFT(p_gridref, 1) IN ('H', 'J') THEN 1000000.0
		END +
		CASE
		WHEN SUBSTR(p_gridref, 2, 1) IN ('A', 'B', 'C', 'D', 'E') THEN 400000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('F', 'G', 'H', 'J', 'K') THEN 300000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('L', 'M', 'N', 'O', 'P') THEN 200000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('Q', 'R', 'S', 'T', 'U') THEN 100000.0
		WHEN SUBSTR(p_gridref, 2, 1) IN ('V', 'W', 'X', 'Y', 'Z') THEN      0.0
		END +
		CAST(LEFT(CONCAT(RIGHT(p_gridref, LENGTH(p_gridref)/2-1), '50000'), 5) AS SIGNED INTEGER);
	DECLARE a    DOUBLE DEFAULT 6377563.396;     
	DECLARE b    DOUBLE DEFAULT 6356256.910;     
	DECLARE F0   DOUBLE DEFAULT 0.9996012717;    
	DECLARE lat0 DOUBLE DEFAULT RADIANS(49.0);   
	DECLARE lon0 DOUBLE DEFAULT RADIANS(-2.0);   
	DECLARE N0   DOUBLE DEFAULT -100000.0;       
	DECLARE E0   DOUBLE DEFAULT  400000.0;       
	DECLARE e2   DOUBLE DEFAULT 1.0-(b*b)/(a*a); 
	DECLARE n    DOUBLE DEFAULT (a-b)/(a+b);
	DECLARE n2   DOUBLE DEFAULT n*n;
	DECLARE n3   DOUBLE DEFAULT n*n*n;
	DECLARE lat  DOUBLE DEFAULT lat0;
	DECLARE M    DOUBLE DEFAULT 0.0;

	REPEAT
		SET lat := (NORTHING-N0-M) / (a*F0) + lat;
		SET M   := b * F0 * (
			((1.0 + n + (5.0/4.0) * n2 + (5.0/4.0) * n3) * (lat - lat0)) -
			((3.0 * n + 3.0 * n * n + (21.0/8.0) * n3) * SIN(lat - lat0) * COS(lat + lat0)) +
			(((15.0/8.0) * n2 + (15.0/8.0) * n3) * SIN(2.0 * (lat - lat0)) * COS(2.0 * (lat + lat0))) -
			((35.0/24.0) * n3 * SIN(3.0 * (lat - lat0)) * COS(3.0 * (lat + lat0)))
		);
	UNTIL NORTHING-N0-M < 0.0001 END REPEAT; 

	BEGIN
		DECLARE cosLat  DOUBLE DEFAULT COS(lat);
		DECLARE sinLat  DOUBLE DEFAULT SIN(lat);
		DECLARE nu      DOUBLE DEFAULT a*F0/SQRT(1.0-e2*sinLat*sinLat); 
		DECLARE rho     DOUBLE DEFAULT a*F0*(1.0-e2)/POW(1.0-e2*sinLat*sinLat, 1.5); 
		DECLARE eta2    DOUBLE DEFAULT nu/rho-1.0;
		DECLARE tanLat  DOUBLE DEFAULT TAN(lat);
		DECLARE tan2lat DOUBLE DEFAULT tanLat*tanLat;
		DECLARE tan4lat DOUBLE DEFAULT tan2lat*tan2lat;
		DECLARE tan6lat DOUBLE DEFAULT tan4lat*tan2lat;
		DECLARE secLat  DOUBLE DEFAULT 1.0/cosLat;
		DECLARE nu3     DOUBLE DEFAULT nu*nu*nu;
		DECLARE nu5     DOUBLE DEFAULT nu3*nu*nu;
		DECLARE nu7     DOUBLE DEFAULT nu5*nu*nu;
		DECLARE VII     DOUBLE DEFAULT tanLat/(2.0*rho*nu);
		DECLARE VIII    DOUBLE DEFAULT tanLat/(24.0*rho*nu3)*(5.0+3.0*tan2lat+eta2-9.0*tan2lat*eta2);
		DECLARE IX      DOUBLE DEFAULT tanLat/(720.0*rho*nu5)*(61.0+90.0*tan2lat+45.0*tan4lat);
		DECLARE X       DOUBLE DEFAULT secLat/nu;
		DECLARE XI      DOUBLE DEFAULT secLat/(6.0*nu3)*(nu/rho+2.0*tan2lat);
		DECLARE XII     DOUBLE DEFAULT secLat/(120.0*nu5)*(5.0+28.0*tan2lat+24.0*tan4lat);
		DECLARE XIIA    DOUBLE DEFAULT secLat/(5040.0*nu7)*(61.0+662.0*tan2lat+1320.0*tan4lat+720.0*tan6lat);
		DECLARE dE      DOUBLE DEFAULT EASTING-E0;
		DECLARE dE2     DOUBLE DEFAULT dE*dE;
		DECLARE dE3     DOUBLE DEFAULT dE2*dE;
		DECLARE dE4     DOUBLE DEFAULT dE2*dE2;
		DECLARE dE5     DOUBLE DEFAULT dE3*dE2;
		DECLARE dE6     DOUBLE DEFAULT dE4*dE2;
		DECLARE dE7     DOUBLE DEFAULT dE5*dE2;

		RETURN POINT(
			ROUND(DEGREES(lon0 + X*dE - XI*dE3 + XII*dE5 - XIIA*dE7), 5),
			ROUND(DEGREES(lat - VII*dE2 + VIII*dE4 - IX*dE6        ), 5)
		);
	END;	
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/kml_to_geom //

CREATE FUNCTION /*##*/kml_to_geom(
	p_kml LONGTEXT
) RETURNS geometry
	COMMENT 'Convert a KML geometry into an OpenGIS geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	DECLARE wkt LONGTEXT CHARSET utf8mb4 DEFAULT p_kml;

	SET wkt := REPLACE(wkt, '\r', ' ');
	SET wkt := REPLACE(wkt, '\n', ' ');
	SET wkt := REPLACE(wkt, '\t', ' ');
	WHILE INSTR(wkt, '  ') DO
		SET wkt := REPLACE(wkt, '  ', ' ');
	END WHILE;
	SET wkt := REPLACE(wkt, '> ', '>');
	SET wkt := REPLACE(wkt, ' <', '<');

	SET wkt := SUBSTRING_INDEX(wkt, '</Placemark>', 1);
	SET wkt := SUBSTRING_INDEX(wkt, '<Placemark>', -1);

	CASE
	WHEN INSTR(wkt, '<Point>') THEN
		SET wkt := SUBSTRING_INDEX(wkt, '</coordinates>', 1);
		SET wkt := SUBSTRING_INDEX(wkt, '<coordinates>', -1);
		SET wkt := SUBSTRING_INDEX(wkt, ',', 2);
		SET wkt := CONCAT('POINT(', ROUND(SUBSTRING_INDEX(wkt, ',', 1), 5), ' ', ROUND(SUBSTRING_INDEX(wkt, ',', -1), 5), ')');
	WHEN INSTR(wkt, '<Polygon>') THEN
		SET wkt := SUBSTR(wkt, INSTR(wkt, '<Polygon>'));
		IF INSTR(wkt, '</MultiGeometry>') THEN
			SET wkt := SUBSTR(wkt, 1, INSTR(wkt, '</MultiGeometry>')-1);
		END IF;
		SET wkt := REPLACE(wkt, '<tessellate>1</tessellate>', '');
		SET wkt := REPLACE(wkt, ',', '|');
		SET wkt := REPLACE(wkt, ' ', ',');
		SET wkt := REPLACE(wkt, '|', ' ');
		SET wkt := REPLACE(wkt, ' 0,', ',');
		SET wkt := REPLACE(wkt, ' 0<', '<');
		SET wkt := REPLACE(wkt, '</Polygon><Polygon>',         ',');
		SET wkt := REPLACE(wkt, '<Polygon>',                   '');
		SET wkt := REPLACE(wkt, '</Polygon>',                  '');
		SET wkt := REPLACE(wkt, '<LinearRing><coordinates>',   '(');
		SET wkt := REPLACE(wkt, '</coordinates></LinearRing>', ')');
		SET wkt := REPLACE(wkt, '</outerBoundaryIs><innerBoundaryIs>', ',');
		SET wkt := REPLACE(wkt, '</innerBoundaryIs><innerBoundaryIs>', ',');
		SET wkt := REPLACE(wkt, '<outerBoundaryIs>', '(');
		SET wkt := REPLACE(wkt, '</outerBoundaryIs>', ')');
		SET wkt := REPLACE(wkt, '</innerBoundaryIs>', ')');
		SET wkt := CONCAT('MULTIPOLYGON(', wkt, ')');
	ELSE
		SET wkt := NULL;
	END CASE;

	RETURN GeomFromText(wkt);
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/multipolygon_to_kml //

CREATE FUNCTION /*##*/multipolygon_to_kml(
	p_multipolygon MULTIPOLYGON
) RETURNS TEXT CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert an OpenGIS multipolygon into an OpenGIS geometry a KML geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	DECLARE kml TEXT    DEFAULT '';
	DECLARE n   INTEGER DEFAULT NUMGEOMETRIES(p_multipolygon);

	IF n=1 THEN
		RETURN /*##*/polygon_to_kml(GEOMETRYN(p_multipolygon, n));
	ELSE
		WHILE n>0 DO
			SET kml := CONCAT(/*##*/polygon_to_kml(GEOMETRYN(p_multipolygon, n)), kml);
			SET n   := n-1;
		END WHILE;
		RETURN CONCAT('<MultiGeometry>', kml, '</MultiGeometry>');
	END IF;
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/geom_to_kml //

CREATE FUNCTION /*##*/geom_to_kml(
	p_geometry GEOMETRY
) RETURNS TEXT CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert an OpenGIS geometry into an OpenGIS geometry a KML geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	RETURN CONCAT(
		'<?xml version="1.0" encoding="UTF-8"?>',
		'<kml xmlns="http://www.opengis.net/kml/2.2"><Document><Placemark>',
		'<Style><LineStyle><color>7f0055ff</color><width>3</width></LineStyle><PolyStyle><color>400055ff</color></PolyStyle></Style>',
		
		CASE GEOMETRYTYPE(p_geometry)
		WHEN 'POINT'        THEN /*##*/point_to_kml       (p_geometry)
		WHEN 'LINESTRING'   THEN /*##*/linestring_to_kml  (p_geometry)
		WHEN 'POLYGON'      THEN /*##*/polygon_to_kml     (p_geometry)
		WHEN 'MULTIPOLYGON' THEN /*##*/multipolygon_to_kml(p_geometry)
		END,
		'</Placemark></Document></kml>'
	);
END //

-- fab-utils - Database utilities in MySQL
--
-- Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

DROP FUNCTION IF EXISTS /*##*/linestring_to_kml //

CREATE FUNCTION /*##*/linestring_to_kml(
	p_linestring LINESTRING
) RETURNS TEXT CHARSET utf8mb4 COLLATE utf8mb4_unicode_ci
	COMMENT 'Convert an OpenGIS linestring into an OpenGIS geometry a KML geometry'
	DETERMINISTIC
	CONTAINS SQL
	SQL SECURITY DEFINER
BEGIN
	DECLARE kml TEXT    DEFAULT '';
	DECLARE n   INTEGER DEFAULT NUMPOINTS(p_linestring);
	DECLARE p   POINT;

	WHILE n>0 DO
		SET p   := POINTN(p_linestring, n);
		SET kml := CONCAT(ROUND(X(p), 5), ',', ROUND(Y(p), 5), ' ', kml);
		SET n   := n-1;
	END WHILE;

	RETURN CONCAT('<LinearRing><coordinates>', kml, '</coordinates></LinearRing>');
END //

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

