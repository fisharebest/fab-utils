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

