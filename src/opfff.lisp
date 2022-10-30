;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Parse the Osmosis Polygon File Filter Format

#|
             _
            / \
           /   \
          /  _  \
         /  | |  \
        /   | |   \
       /    | |    \
      /     | |     \
     /      |_|      \
    /        _        \
   /        / \        \
  /         \_/         \
 /                       \
´=========================`
      IN DEVELOPMENT

wget tiles intersecting with poly file: no need to recurse into tile if it does not intersect the poly.
 ... Quadtree insertion of the poly, hashing out directory entries instead of building an index...

THINK: limit layers and locator files through an inner join to the region geometry? The geometry is already in the database (+indexed)...
|#

