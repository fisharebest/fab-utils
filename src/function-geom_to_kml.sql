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

