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

