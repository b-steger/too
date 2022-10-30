;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Definitions from the OpenStreetMap Wiki if *trademark-permission?* is non-nil, own descriptions otherwise.
;;;; Attention (escaping): HTML strings are already rendered.

(defparameter *wiki-definitions*
  ;;name (string) → `wiki-definition
  (make-hash-table :test #'equal))

(defclass wiki-definition ()
  ((name :accessor name :initarg :name :initform nil)
   (paragraphs :accessor paragraphs :initarg :paragraphs :initform nil))
  (:documentation "Extracts from OpenStreetMap wiki articles explaining tags if *trademark-permission?* is non-nil, own articles otherwise."))

(defun register-wiki-definition (name &rest paragraphs)
  "Quick helper function which registers an entry in *wiki-definitions*.
   Side-effecting."
  (setf (gethash name *wiki-definitions*)
        (make-instance
         'wiki-definition
         :name name
         :paragraphs paragraphs)))


;;;; Data

(register-wiki-definition
 "Clinic"
 "A clinic provides specialized medical services. A clinic is mainly differed from general hospitals in that it does not handle general medical emergencies.")

(register-wiki-definition
 "Doctor's office"
 "Except for medical emergencies, for which a hospital with the necessary emergency department is well prepared, a doctor's office is the primary place for medical treatments and consultation.")

(register-wiki-definition
 "Hospital"
 "Hospitals are part of public infrastructure tasked with medical care. Most hospitals contain an emergency department capable of dealing with medical emergencies and unplanned visits.")

(register-wiki-definition
 "Police station"
 "Police stations are part of public infrastructure tasked with providing police services. Generally, they are responsible for a certain area/district/precinct."
 "Specializations such as women's police stations exist. Officers are only allowed to process certain crimes such as threats against women or violence against women. They also provide counseling, inform about the rights women have, and may recommend other institutions.")

(register-wiki-definition
 "Social facility"
 "There are several types of social facilities:"
 (x-ul (x-li "group_home: A group home is a supervised space that provides housing and food. Examples include orphanages, safe spaces for children or adolescents in stress situations and supervised homes for elderly.")
       (x-li "nursing_home: A home for persons in need of assistance.")
       (x-li "hospice: A hopsice is a nursing home for terminally ill persons.")
       (x-li "assisted_living: A nursing home where assistance is provided, but only upon request.")
       (x-li "day_care: Shares similarities with nursing homes, but services are only provided during the day, i.e. people visit those places during the day but sleep elsewhere.")
       (x-li "shelter: A shelter provides beds for sleeping, intended for emergency situations.")
       (x-li "ambulatory_care: Buildings designed to help social service workers do their job.")
       (x-li "outreach: Properties used by organizations who help with finding jobs/housing/legal advice etc. for certain groups.")
       (x-li "workshop: Properties used by rehabilitation programs. Also includes workshops where persons with disabilities work.")
       (x-li "clothing_bank: A space where used clothing is offered for free or for a very small price.")
       (x-li "food_bank: Food is distributed for free or sold for a very small price here.")
       (x-li "soup_kitchen: A place where meals are served for free or for a very small price.")
       (x-li "dairy_kitchen: Distribution of dairy food to certain eligible groups.")))

(register-wiki-definition
 "Emergency ward entrance"
 "An emergency ward entrance marks the location of an emergency department within an hospital. It is typically the location where ambulance vehicles arrive.")

(register-wiki-definition
 "City"
 "A city is a large aggregation of buildings and is typically the cultural and economic center for its surrounding area."
 "More than 50% of the world's population lives in cities.")

(register-wiki-definition
 "Hamlet"
 "Hamlets are isolated groups of buildings that grew historically and naturally, but did not attract much inhabitants."
 "Interestingly, because hamlets are isolated and because the focus is on agriculture, its inhabitants are typically self-supporters, different to small villages where other economic sectors are found, too.")

(register-wiki-definition
 "Neighbourhood"
 "A neighbourhood is the immediate area surrounding a building within a settlement. Neighbourhoods have some sort of identifying similarities such as similar architectural designs, but are mainly conceptualized as the buildings and humans around oneself's home.")

(register-wiki-definition
 "Quarter"
 "The industrial revolution introduced some sort of anonymity to cities and towns. Quarters are informal districts within large settlements where the local community managed to organize a feeling of belonging.")

(register-wiki-definition
 "Suburb"
 "Suburbs are former villages that got included in towns or cities due to urban sprawl.")

(register-wiki-definition
 "Town"
 "A town is an aggregation of buildings, just like cities, but typically is not as large and important as a city."
 "Towns provide services and house businesses that are not found in their agglomeration, and are therefore an important center for the region, too. Nevertheless, towns are not able to compete with the importance and influence of cities.")

(register-wiki-definition
 "Village"
 "Villages is a smaller aggregation of buildings than towns and are typically found in rural areas."
 "Most villages do not have the infrastructure for all the needs of its population. In fact, some villages only serve as &quot;sleeping villages&quot; since habitants work and live in the city during the day.")

(register-wiki-definition
 "Medical supply store"
 "Stores selling health-related equipment for personal use such as crutches, rollator walkers and alike.")

(register-wiki-definition
 "Pharmacy"
 "A pharmacy is a shop specializing in pharmaceutical drugs/medicaments."
 "Pharmacies may also offer related health-related equipment for personal use.")

