-- fab-genealogy - Database utilities in MySQL
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

