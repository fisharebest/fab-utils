# fab-utils - Database utilities in MySQL
#
# Copyright (c) 2013 Greg Roach, fisharebest@gmail.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

TEST_DB=fab_utils

fab-utils.sql: src/connection.sql src/function-*.sql src/procedure-*.sql
	cat $^ > $@

fab-utils-test.sql: fab-utils.sql test/*.sql
	sed -e 's/\/\*##\*\//fab_/g' $^ > $@

.PHONY: $(TEST_DB)
$(TEST_DB): fab-utils-test.sql
	mysql --execute 'DROP DATABASE IF EXISTS `$(TEST_DB)`'
	mysql --execute 'CREATE DATABASE `$(TEST_DB)`'
	mysql --database $(TEST_DB) --execute 'SOURCE $<'

test.log: $(TEST_DB) fab-utils-test.sql
	script -q -c "mysql --database \"$(TEST_DB)\" --execute \"CALL fab_unit.run('$(TEST_DB)', 'test_%')\" " $@

clean:
	rm -f fab-utils.sql fab-utils-test.sql test.log

