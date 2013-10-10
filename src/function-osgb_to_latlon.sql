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

