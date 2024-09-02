#!/usr/bin/env bash

# Joe's Cloud Infra
# Copyright (C) 2024  Josep Jesus Bigorra Algaba (jjbigorra@gmail.com)

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


echo "Starting the grabbing and restoring of a WikiMusic backup"

export AWS_PROFILE=master-infra

echo "Fetching latest backup file for WikiMusic"
DB_BACKUP_FILE=$(aws s3 ls s3://cloud-infra-state-jjba/wikimusic/backups/postgresql/ | grep wikimusic_database_ | sort | tail -n 1 | awk '{print $4}')

aws s3 cp s3://cloud-infra-state-jjba/wikimusic/backups/postgresql/$DB_BACKUP_FILE $HOME/$DB_BACKUP_FILE


echo "Restoring from latest database backup"
export PGPASSWORD=wikimusic_admin
nix-shell -p postgresql \
	  --run "psql -U wikimusic_admin wikimusic_database -p 55432 -h localhost < $HOME/$DB_BACKUP_FILE"

