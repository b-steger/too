;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

;;;; Queries adapted from osm2pgsql, see legal/osm2pgsql.txt.
;;;; Columns adapted from openstreetmap-carto/openstreetmap-carto.style, see legal/openstreetmap-carto.txt

(in-package :too)

#|(time (length (with-open-file (file "/path/to/switzerland.pbf"
                      :direction :input
                      :element-type '(unsigned-byte 8))
  (osmpbf:get-next-primitive-groups file))))|#

;;;; Rearrange the SQL queries that osm2pgsql sends to the server after it has filled the tables.
;;;;  Goal: reduce the disk usage spike of table clustering before the geometry index and other indices get calculated.
;;;; Additionally includes indices for layers.

(defun osm2pgsql-tablename (type)
  "TYPE: (or :point :line :polygon :roads)"
  (format nil "planet_osm_~(~A~)" type))

(defun osm2pgsql-cluster-way (&key (tablespace "osmbasenvme") type)
  "Cluster table by geometry.
   TYPE: (or :point :line :polygon :roads)"
  (let ((table (osm2pgsql-tablename type)))
    (list (format nil "CREATE TABLE \"~A_tmp\"  TABLESPACE \"~A\" AS SELECT * FROM \"~A\" WHERE ST_IsValid(way) ORDER BY way" table tablespace table)
          (format nil "DROP TABLE \"~A\"" table)
          (format nil "ALTER TABLE \"~A_tmp\" RENAME TO \"~A\"" table table))))

(defun osm2pgsql-index-way (&key (tablespace "osmbasenvme") type)
  "Create geometry index on table."
  (let ((table (osm2pgsql-tablename type)))
    (list (format nil "CREATE INDEX ON \"~A\" USING GIST (way) WITH (fillfactor = 100) TABLESPACE \"~A\"" table tablespace))))

(defparameter *table-tag-columns*
  (list
   :point (list "access" "addr:housename" "addr:housenumber" "admin_level" "aerialway" "aeroway" "amenity" "barrier" "boundary" "building" "highway" "historic" "junction" "landuse" "layer" "leisure" "lock" "man_made" "military" "name" "natural" "oneway" "place" "power" "railway" "ref" "religion" "shop" "tourism" "water" "waterway")
   :line (list "access" "addr:housename" "addr:housenumber" "addr:interpolation" "admin_level" "aerialway" "aeroway" "amenity" "barrier" "bicycle" "boundary" "bridge" "building" "construction" "covered" "foot" "highway" "historic" "horse" "junction" "landuse" "layer" "leisure" "lock" "man_made" "military" "name" "natural" "oneway" "place" "power" "railway" "ref" "religion" "route" "service" "shop" "surface" "tourism" "tracktype" "tunnel" "water" "waterway" "way_area" "z_order")
   :polygon (list "access" "addr:housename" "addr:housenumber" "addr:interpolation" "admin_level" "aerialway" "aeroway" "amenity" "barrier" "bicycle" "bridge" "boundary" "building" "construction" "covered" "foot" "highway" "historic" "horse" "junction" "landuse" "layer" "leisure" "lock" "man_made" "military" "name" "natural" "oneway" "place" "power" "railway" "ref" "religion" "route" "service" "shop" "surface" "tourism" "tracktype" "tunnel" "water" "waterway" "way_area" "z_order")
   :roads (list "access" "addr:housename" "addr:housenumber" "addr:interpolation" "admin_level" "aerialway" "aeroway" "amenity" "barrier" "bicycle" "bridge" "boundary" "building" "construction" "covered" "foot" "highway" "historic" "horse" "junction" "landuse" "layer" "leisure" "lock" "man_made" "military" "name" "natural" "oneway" "place" "power" "railway" "ref" "religion" "route" "service" "shop" "surface" "tourism" "tracktype" "tunnel" "water" "waterway" "way_area" "z_order")))

(defun osm2pgsql-index-columns (&key (tablespace "osmbasenvme") type)
  (let ((table (osm2pgsql-tablename type)))
    (asplice
     ;;Useful for diffs, but decreases rendering time, too.
     (format nil "CREATE INDEX ON \"~A\" USING BTREE (osm_id) WITH (fillfactor = 100) TABLESPACE \"~A\"" table tablespace)
     ;;hstore in general.
     (format nil "CREATE INDEX ON \"~A\" USING GIN (tags) WITH (fillfactor = 100) TABLESPACE \"~A\"" table tablespace)
     ;;Tags from openstreetmap-carto/project.mml (searched for "tags"). If the indices use too much space or introduce too many random accesses, aim for golf location entrance indoor protect_class ford attraction capital addr:unit addr:flats, which are used as selectors.
     ;;Using three parallel Bashs proved useful.
     (loop for tag in (sort (getf (list
                                   ;;TOO: emergency government healthcare toilets
                                   
                                   ;;"station" "denomination" "generator:source" "office" "operator" "parking" "vending" "icao" "iata"
                                   :point (list "entrance" "indoor" "population" "capital" "ele" "information" "mountain_pass" "height" "parking" "vending" "office" "diplomatic" "advertising" "emergency" "location" "ford" "protect_class" "golf" "recycling_type" "tower:construction" "tower:type" "telescope:type" "telescope:diameter" "castle_type" "sport" "memorial" "artwork_type" "operator" "addr:unit" "addr:flats" "government" "healtcare" "toilets")
                                   ;;"substance" "highspeed" "usage" "operator"
                                   :line (list "intermittent" "seasonal" "substance" "location" "golf" "ford" "attraction" "lock_name" "emergency" "government" "healthcare" "toilets")
                                   ;;"public_transport" "leaf_type" "station" "denomination" "generator:source" "office" "operator" "parking" "vending" "wetland" "tower:type" "icao" "iata"
                                   ;;Unused: "admin_level" "golf" "parking" "location" "intermittent" "seasonal" "basin" "protect_class" "station" "ele" "information" "mountain_pass" "height" "vending" "office" "diplomatic" "advertising" "emergency" "ford" "recycling_type" "tower:construction" "tower:type" "telescope:type" "telescope:diameter" "castle_type" "sport" "memorial" "artwork_type" "operator" "addr:unit" "addr:flats"
                                   :polygon (list "emergency" "government" "healthcare" "toilets" "capital" "population")
                                   :roads nil)
                                  type)
                            #'string<)
           collect (format nil "CREATE INDEX \"~A_tag_~A_idx\" ON \"~A\" ((tags -> '~A')) WITH (fillfactor = 100) TABLESPACE \"~A\" WHERE tags->'~A' IS NOT NULL" table tag table tag tablespace tag))
     ;;Index every column.
     (loop for column
           ;;:polygon: Of the remaining indices, only amenity is fetched, others are scanned.
             in (sort (set-difference
                       (getf *table-tag-columns* type)
                       ;;Unused.
                       ;; "highway" "railway" "aeroway" "aerialway"
                       (when (eql :polygon type)
                         (list "access" "addr:housename" "addr:housenumber" "addr:interpolation" "barrier" "bicycle" "bridge" "construction" "covered" "historic" "horse" "junction" "landuse" "layer" "leisure" "lock" "man_made" "military" "name" "natural" "oneway" "power" "ref" "religion" "route" "service" "surface" "tourism" "tracktype" "tunnel" "water" "waterway" "way_area" "z_order"))
                       :test #'string=)
                      #'string<)
           ;;THINK: index-only scans? → the bigger problem are the concats in the queries (the column name defines the tag type already) ← measure that.
           collect (format nil "CREATE INDEX ON \"~A\" (\"~A\") WITH (fillfactor = 100) TABLESPACE \"~A\" WHERE \"~A\" IS NOT NULL" table column tablespace column)))))

(defun osm2pgsql-analyze (&key type)
  "Analyze table."
  (let ((table (osm2pgsql-tablename type)))
    (list (format nil "ANALYZE \"~A\"" table))))

(defun osm2pgsql-opencarto-indexes (&key type)
  "TYPE: like in #'osm2pgsql-tablename."
  (case type
    (:line (list "CREATE INDEX planet_osm_line_ferry
  ON planet_osm_line USING GIST (way) WITH (fillfactor = 100)
  WHERE route = 'ferry' AND osm_id > 0"
                 "CREATE INDEX planet_osm_line_label
  ON planet_osm_line USING GIST (way) WITH (fillfactor = 100)
  WHERE name IS NOT NULL OR ref IS NOT NULL"
                 "CREATE INDEX planet_osm_line_river
  ON planet_osm_line USING GIST (way) WITH (fillfactor = 100)
  WHERE waterway = 'river'"
                 "CREATE INDEX planet_osm_line_waterway
  ON planet_osm_line USING GIST (way) WITH (fillfactor = 100)
  WHERE waterway IN ('river', 'canal', 'stream', 'drain', 'ditch')"))
    (:point (list "CREATE INDEX planet_osm_point_place
  ON planet_osm_point USING GIST (way) WITH (fillfactor = 100)
  WHERE place IS NOT NULL AND name IS NOT NULL"))
    (:polygon (list "CREATE INDEX planet_osm_polygon_admin
  ON planet_osm_polygon USING GIST (ST_PointOnSurface(way)) WITH (fillfactor = 100)
  WHERE name IS NOT NULL AND boundary = 'administrative' AND admin_level IN ('0', '1', '2', '3', '4')"
   "CREATE INDEX planet_osm_polygon_military
  ON planet_osm_polygon USING GIST (way) WITH (fillfactor = 100)
  WHERE (landuse = 'military' OR military = 'danger_area') AND building IS NULL"
   "CREATE INDEX planet_osm_polygon_name
  ON planet_osm_polygon USING GIST (ST_PointOnSurface(way)) WITH (fillfactor = 100)
  WHERE name IS NOT NULL"
   "CREATE INDEX planet_osm_polygon_nobuilding
  ON planet_osm_polygon USING GIST (way) WITH (fillfactor = 100)
  WHERE building IS NULL"
   "CREATE INDEX planet_osm_polygon_water
  ON planet_osm_polygon USING GIST (way) WITH (fillfactor = 100)
  WHERE waterway IN ('dock', 'riverbank', 'canal')
    OR landuse IN ('reservoir', 'basin')
    OR \"natural\" IN ('water', 'glacier')"
   "CREATE INDEX planet_osm_polygon_way_area_z10
  ON planet_osm_polygon USING GIST (way) WITH (fillfactor = 100)
  WHERE way_area > 23300"
   "CREATE INDEX planet_osm_polygon_way_area_z6
  ON planet_osm_polygon USING GIST (way) WITH (fillfactor = 100)
  WHERE way_area > 5980000"))
    (:roads (list "CREATE INDEX planet_osm_roads_admin
  ON planet_osm_roads USING GIST (way) WITH (fillfactor = 100)
  WHERE boundary = 'administrative'"
   "CREATE INDEX planet_osm_roads_admin_low
  ON planet_osm_roads USING GIST (way) WITH (fillfactor = 100)
  WHERE boundary = 'administrative' AND admin_level IN ('0', '1', '2', '3', '4')"
   "CREATE INDEX planet_osm_roads_roads_ref ON planet_osm_roads
  USING GIST (way) WITH (fillfactor = 100) WHERE highway IS NOT NULL AND ref IS NOT NULL"))))

(defun wrap-sql-in-bash (sql-command)
  "SQL-command: string or a sql-primitive."
  (dsl " "
       "echo \"$(date) \" && time"
       "psql"
       (list "--echo-all"
             "--dbname=gis"
             "-c"
             ;;#'escape-js: OK for this situation ("addr:housenumber" is the worst case).
             (dsl :quote (escape-js (substitute #\Space #\Newline sql-command))))))
(wrap-sql-in-bash "select * from \"r\"")

(defun osm2pgsql-sql-commands (&key (path "/dev/shm/temp") (tablespace "osmbasenvme")
                                 (cluster? T) (geometry-index? T) (analyze? T)
                                 (bash-layer? T) (order '(:step :table)))
  "Print the SQL commands that osm2pgsql executes after loading.
   Ordering by table first may make use of caches, whereas ordering by step first is easier to manage."
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* ((commands (loop for type in (list :point :line :polygon :roads)
                           collect (remove nil (list
                                                (when cluster?
                                                  (osm2pgsql-cluster-way :tablespace tablespace :type type))
                                                (when geometry-index?
                                                  (osm2pgsql-index-way :tablespace tablespace :type type))
                                                (when analyze?
                                                  (osm2pgsql-analyze :type type))
                                                (osm2pgsql-index-columns :tablespace tablespace :type type)
                                                (osm2pgsql-opencarto-indexes :type type)))))
           (sorted-and-flatted (flatten
                                (cond ((equal order '(:table :step))
                                       commands)
                                      ((equal order '(:step :table))
                                       (apply #'mapcar #'list commands))
                                      (T (error "Unknown order"))))))
      (loop for command in sorted-and-flatted
            append (if bash-layer?
                       (format stream "~A~%" (wrap-sql-in-bash command))
                       (format stream "~A;~%" command))))))
(osm2pgsql-sql-commands :cluster? T :geometry-index? T :analyze? T :bash-layer? T :order '(:step :table))

