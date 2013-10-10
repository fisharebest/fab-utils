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

