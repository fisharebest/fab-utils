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

