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

