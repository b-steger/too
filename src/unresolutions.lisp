;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; A selection of resolutions of the United Nations General Assembly (UNGA; public domain)
;;;;  Half-materialized as HTML, the first citizen in ZIM files.

(defun resolution-template (title-header title own-filename original-url &rest body)
  "The original resolution as PDF file entry and a converted HTML version."
  (list (dir-entry
         :url own-filename
         :title (format nil "~A (original PDF)" title)
         :mimetype-id :pdf
         :relative-path own-filename)
        (dir-entry
         :url original-url
         :title title
         :mimetype-id :html
         :content (x-html
                   (x-head (x-title title)
                           "<meta charset='utf8'>")
                   (x-body
                    (x-i "Converted from " (x-a :href own-filename "the original") ".")
                    (x-hr)
                    (x-h1 title-header)
                    (x-h2 title)
                    body)))))

(defun resq (in-italics &rest body)
  "Quick helper for a typical header, preambular or operative resolution clause.
   Introduces a #\Space after IN-ITALICS when BODY is non-nil."
  (x-p (x-i in-italics)
       (when body #\Space)
       body))

(defun a-res-68-262 ()
  (resolution-template
   "Resolution adopted by the General Assembly on 27 March 2014"
   "68/262. Territorial integrity of Ukraine"
   "un/A_RES_68_262-EN.pdf"
   "A_RES_68_262-EN.html"
   (resq "The General Assembly,")
   (resq "Reaffirming" "the paramount importance of the Charter of the United Nations in the promotion of the rule of law among nations,")
   (resq "Recalling" "the obligations of all States under Article 2 of the Charter to refrain in their international relations from the threat or use of force against the territorial integrity or political independence of any State, and to settle their international disputes by peaceful means,")
   (resq "Recalling also" "its resolution 2625 (XXV) of 24 October 1970, in which it approved the Declaration on Principles of International Law concerning Friendly Relations and Cooperation among States in accordance with the Charter of the United Nations, and reaffirming the principles contained therein that the territory of a State shall not be the object of acquisition by another State resulting from the threat or use of force, and that any attempt aimed at the partial or total disruption of the national unity and territorial integrity of a State or country or at its political independence is incompatible with the purposes and principles of the Charter,")
   (resq "Recalling further" "the Final Act of the Conference on Security and Cooperation in Europe, signed in Helsinki on 1 August 1975, the Memorandum on Security Assurances in Connection with Ukraine's Accession to the Treaty on the Non-Proliferation of Nuclear Weapons (Budapest Memorandum) of 5 December 1994, the Treaty on Friendship, Cooperation and Partnership between Ukraine and the Russian Federation of 31 May 1997 and the Alma-Ata Declaration of 21 December 1991,")
   (resq "Stressing" "the importance of maintaining the inclusive political dialogue in Ukraine that reflects the diversity of its society and includes representation from all parts of Ukraine,")
   (resq "Welcoming" "the continued efforts by the Secretary-General and the Organization for Security and Cooperation in Europe and other international and regional organizations to support de-escalation of the situation with respect to Ukraine,")
   (resq "Noting" "that the referendum held in the Autonomous Republic of Crimea and the city of Sevastopol on 16 March 2014 was not authorized by Ukraine,")
   (x-ol (x-li (resq "Affirms" "its commitment to the sovereignty, political independence, unity and territorial integrity of Ukraine within its internationally recognized borders;"))
         (x-li (resq "Calls upon" "all States to desist and refrain from actions aimed at the partial or total disruption of the national unity and territorial integrity of Ukraine, including any attempts to modify Ukraine’s borders through the threat or use of force or other unlawful means;"))
         (x-li (resq "Urges" "all parties to pursue immediately the peaceful resolution of the situation with respect to Ukraine through direct political dialogue, to exercise restraint, to refrain from unilateral actions and inflammatory rhetoric that may increase tensions and to engage fully with international mediation efforts;"))
         (x-li (resq "Welcomes" "the efforts of the United Nations, the Organization for Security and Cooperation in Europe and other international and regional organizations to assist Ukraine in protecting the rights of all persons in Ukraine, including the rights of persons belonging to minorities;"))
         (x-li (resq "Underscores" "that the referendum held in the Autonomous Republic of Crimea and the city of Sevastopol on 16 March 2014, having no validity, cannot form the basis for any alteration of the status of the Autonomous Republic of Crimea or of the city of Sevastopol;"))
         (x-li (resq "Calls upon" "all States, international organizations and specialized agencies not to recognize any alteration of the status of the Autonomous Republic of Crimea and the city of Sevastopol on the basis of the above-mentioned referendum and to refrain from any action or dealing that might be interpreted as recognizing any such altered status.")))))
(a-res-68-262)

(defun a-res-es-11-1 ()
  (resolution-template
   "Resolution adopted by the General Assembly on 2 March 2022"
   "ES-11/1. Aggression against Ukraine"
   "un/A_RES_ES-11_1-EN.pdf"
   "A_RES_ES-11_1-EN.html"
   (resq "The General Assembly,")
   (resq "Reaffirming" "the paramount importance of the Charter of the United Nations in the promotion of the rule of law among nations,")
   (resq "Recalling" "the obligation of all States under Article 2 of the Charter to refrain in their international relations from the threat or use of force against the territorial integrity or political independence of any State, or in any other manner inconsistent with the purposes of the United Nations, and to settle their international disputes by peaceful means,")
   (resq "Recalling also" "the obligation under Article 2 (2) of the Charter, that all Members, in order to ensure to all of them the rights and benefits resulting from membership, shall fulfil in good faith the obligations assumed by them in accordance with the Charter,")
   (resq "Taking note" "of Security Council resolution 2623 (2022) of 27 February 2022, in which the Council called for an emergency special session of the General Assembly to examine the question contained in document S/Agenda/8979,")
   (resq "Recalling" "General Assembly resolution 377 A (V) of 3 November 1950, entitled &quot;Uniting for peace&quot;, and taking into account that the lack of unanimity of the permanent members of the Security Council at its 8979th meeting has prevented it from exercising its primary responsibility for the maintenance of international peace and security,")
   (resq "Recalling also" "its resolution 2625 (XXV) of 24 October 1970, in which it approved the Declaration on Principles of International Law concerning Friendly Relations and Cooperation among States in accordance with the Charter of the United Nations, and reaffirming the principles contained therein that the territory of a State shall not be the object of acquisition by another State resulting from the threat or use of force, and that any attempt aimed at the partial or total disruption of the national unity and territorial integrity of a State or country or at its political independence is incompatible with the purposes and principles of the Charter,")
   (resq "Recalling further" "its resolution 3314 (XXIX) of 14 December 1974, which defines aggression as the use of armed force by a State against the sovereignty, territorial integrity or political independence of another State, or in any other manner inconsistent with the Charter,")
   (resq "Bearing in mind" "the importance of maintaining and strengthening international peace founded upon freedom, equality, justice and respect for human rights and of developing friendly relations among nations irrespective of their political, economic and social systems or the levels of their development,")
   (resq "Recalling" "the Final Act of the Conference on Security and Cooperation in Europe, signed in Helsinki on 1 August 1975, and the Memorandum on Security Assurances in Connection with Ukraine's Accession to the Treaty on the Non-Proliferation of Nuclear Weapons (Budapest Memorandum) of 5 December 1994,")
   (resq "Condemning" "the 24 February 2022 declaration by the Russian Federation of a &quot;special military operation&quot; in Ukraine,")
   (resq "Reaffirming" "that no territorial acquisition resulting from the threat or use of force shall be recognized as legal,")
   (resq "Expressing grave concern" "at reports of attacks on civilian facilities such as residences, schools and hospitals, and of civilian casualties, including women, older persons, persons with disabilities, and children,")
   (resq "Recognizing" "that the military operations of the Russian Federation inside the sovereign territory of Ukraine are on a scale that the international community has not seen in Europe in decades and that urgent action is needed to save this generation from the scourge of war,")
   (resq "Endorsing" "the Secretary-General's statement of 24 February 2022 in which he recalled that the use of force by one country against another is the repudiation of the principles that every country has committed to uphold and that the present military offensive of the Russian Federation is against the Charter,")
   (resq "Condemning" "the decision of the Russian Federation to increase the readiness of its nuclear forces,")
   (resq "Expressing grave concern" "at the deteriorating humanitarian situation in and around Ukraine, with an increasing number of internally displaced persons and refugees in need of humanitarian assistance,")
   (resq "Expressing concern also" "about the potential impact of the conflict on increased food insecurity globally, as Ukraine and the region are one of the world's most important areas for grain and agricultural exports, when millions of people are facing famine or the immediate risk of famine or are experiencing severe food insecurity in several regions of the world, as well as on energy security,")
   (resq "Welcoming" "the continued efforts by the Secretary-General and the Organization for Security and Cooperation in Europe and other international and regional organizations to support de-escalation of the situation with respect to Ukraine, and encouraging continued dialogue,")
   (x-ol (x-li (resq "Reaffirms its commitment" "to the sovereignty, independence, unity and territorial integrity of Ukraine within its internationally recognized borders, extending to its territorial waters;"))
         (x-li (resq "Deplores in the strongest terms" "the aggression by the Russian Federation against Ukraine in violation of Article 2 (4) of the Charter;"))
         (x-li (resq "Demands" "that the Russian Federation immediately cease its use of force against Ukraine and to refrain from any further unlawful threat or use of force against any Member State;"))
         (x-li (resq "Also demands" "that the Russian Federation immediately, completely and unconditionally withdraw all of its military forces from the territory of Ukraine within its internationally recognized borders;"))
         (x-li (resq "Deplores" "the 21 February 2022 decision by the Russian Federation related to the status of certain areas of the Donetsk and Luhansk regions of Ukraine as a violation of the territorial integrity and sovereignty of Ukraine and inconsistent with the principles of the Charter;"))
         (x-li (resq "Demands" "that the Russian Federation immediately and unconditionally reverse the decision related to the status of certain areas of the Donetsk and Luhansk regions of Ukraine;"))
         (x-li (resq "Calls upon" "the Russian Federation to abide by the principles set forth in the Charter and the Declaration on Friendly Relations;"))
         (x-li (resq "Calls upon" "the parties to abide by the Minsk agreements and to work constructively in relevant international frameworks, including in the Normandy format and Trilateral Contact Group, towards their full implementation;"))
         (x-li (resq "Demands" "all parties to allow safe and unfettered passage to destinations outside of Ukraine and to facilitate the rapid, safe and unhindered access to humanitarian assistance for those in need in Ukraine, to protect civilians, including humanitarian personnel and persons in vulnerable situations, including women, older persons, persons with disabilities, indigenous peoples, migrants and children, and to respect human rights;"))
         (x-li (resq "Deplores" "the involvement of Belarus in this unlawful use of force against Ukraine, and calls upon it to abide by its international obligations;"))
         (x-li (resq "Condemns" "all violations of international humanitarian law and violations and abuses of human rights, and calls upon all parties to respect strictly the relevant provisions of international humanitarian law, including the Genev a Conventions of 1949 and Additional Protocol I thereto of 1977, as applicable, and to respect international human rights law, and in this regard further demands that all parties ensure respect for and the protection of all medical personnel and humanitarian personnel exclusively engaged in medical duties, their means of transport and equipment, as well as hospitals and other medical facilities;"))
         (x-li (resq "Demands" "that all parties fully comply with their obligations under international humanitarian law to spare the civilian population, and civilian objects, refraining from attacking, destroying, removing or rendering useless objects indispensable to the survival of the civilian population, and respecting and protecting humanitarian personnel and consignments used for humanitarian relief operations;"))
         (x-li (resq "Requests" "the Emergency Relief Coordinator to provide, 30 days after the adoption of the present resolution, a report on the humanitarian situation in Ukraine and on the humanitarian response;"))
         (x-li (resq "Urges" "the immediate peaceful resolution of the conflict between the Russian Federation and Ukraine through political dialogue, negotiations, mediation and other peaceful means;"))
         (x-li (resq "Welcomes and urges" "the continued efforts by the Secretary-General, Member States, the Organization for Security and Cooperation in Europe and other international and regional organizations to support the de -escalation of the current situation, as well as the efforts of the United Nations, including of the United Nations Crisis Coordinator for Ukraine, and humanitarian organizations to respond to the humanitarian and refugee crisis that the aggression by the Russian Federation has created;"))
         (x-li (resq "Decides" "to adjourn the eleventh emergency special session of the General Assembly temporarily and to authorize the President of the General Assembly to resume its meetings upon request from Member States.")))))
(a-res-es-11-1)

(defun a-res-es-11-2 ()
  (resolution-template
   "Resolution adopted by the General Assembly on 24 March 2022"
   "Resolution adopted by the General Assembly on 24 March 2022"
   "un/A_RES_ES-11_2-EN.pdf"
   "A_RES_ES-11_2-EN.html"
   (resq "The General Assembly,")
   (resq "Reaffirming its determination" "to save succeeding generations from the scourge of war,")
   (resq "Reaffirming" "its resolutions 46/182 of 19 December 1991 and 76/124 of 10 December 2021,")
   (resq "Recalling" "the obligation of all States under Article 2 of the Charter of the United Nations to refrain in their international relations from the threat or use of force against the territorial integrity or political independence of any State, or in any other manner inconsistent with the purposes of the United Nations, and to settle their international disputes by peaceful means,")
   (resq "Reaffirming its commitment" "to the sovereignty, independence, unity and territorial integrity of Ukraine within its internationally recognized borders, extending to its territorial waters,")
   (resq "Recognizing" "that the military offensive of the Russian Federation inside the sovereign territory of Ukraine and its humanitarian consequences are on a scale that the international community has not seen in Europe in decades,")
   (resq "Reiterating" "the call of the Secretary-General to the Russian Federation to stop its military offensive, as well as his call to establish a ceasefire and to return to the path of dialogue and negotiations,")
   (resq "Recalling its demand" "that the Russian Federation immediately, completely and unconditionally withdraw all of its military forces from the territory of Ukraine within its internationally recognized borders,")
   (resq "Deploring" "the dire humanitarian consequences of the hostilities by the Russian
Federation against Ukraine, including the besiegement of and shelling and air strikes in densely populated cities of Ukraine, in particular Mariupol, as well as attacks striking civilians, including journalists, and civilian objects, in particular schools and other educational institutions, water and sanitation systems, medical facilities and their means of transport and equipment, and the abduction of local officials, as well as attacks striking diplomatic premises and cultural sites,")
   (resq "Expressing grave concern" "at the deteriorating humanitarian situation in and around Ukraine, in particular at the high number of civilian casualties, including women and children, and the increasing number of internally displaced persons and refugees in need of humanitarian assistance,")
   (resq "Reaffirming" "the need to protect, without discrimination of any kind, the safety, dignity, human rights and fundamental freedoms of people fleeing the conflict and violence, regardless of their status, while promoting the security and prosperity of all communities, and condemning in this regard any acts, manifestations and expressions of racism, racial discrimination, xenophobia and related intolerance against people on the move, including refugees,")
   (resq "Strongly condemning" "any attacks directed against civilians as such and other protected persons and civilian objects, including civilian evacuation convoys, as well as indiscriminate and disproportionate attacks, including indiscriminate shelling and the indiscriminate use of explosive weapons, and further expressing concern about the long-term risks posed by damage to civilian infrastructure and unexploded ordnance to the civilian population,")
   (resq "Stressing" "the particular impact that armed conflict has on women and children, including as refugees and internally displaced persons, and other civilians who have specific needs, including persons with disabilities and older persons, and stressing also the need to ensure safe passage, as well as protection and assistance, to all affected civilian populations,")
   (resq "Expressing its deep appreciation" "for the significant and admirable efforts that have been made by neighbouring countries to accommodate refugees,")
   (resq "Expressing concern" "about the impact of the conflict on increased food insecurity globally, in particular in the least developed countries, as Ukraine and the region are one of the world’s most important areas for grain and agricultural exports, when millions of people are facing famine or the immediate risk of famine or are experiencing severe food insecurity in several regions of the world, as well as on energy security,")
   (resq "Recalling" "the link between armed conflict and violence and conflict-induced food insecurity and the threat of famine, and stressing in this regard that armed conflict, violations of international humanitarian law and international human rights law, and food insecurity can be drivers of forced displacement and that, conversely, forced displacement in countries in armed conflict can have a devastating impact on agricultural production and livelihoods,")
   (resq "Expressing concern" "about the grave humanitarian consequences of a possible accident resulting from the bombing and shelling of the Ukrainian nuclear infrastructure, reiterating the obligation to ensure the safety and security of all nuclear infrastructure, and expressing concern about the impact of the conflict on the environment,")
   (resq "Recalling" "the obligation of all States and parties to an armed conflict to fully respect international humanitarian law, in particular the principles of distinction and proportionality and the obligation to take all feasible precautions to avoid and in any event minimize harm to civilians and damage to civilian objects, reiterating that sieges, the purpose of which is to starve the civilian populations, are a violation of international humanitarian law, and urging all States and parties to armed conflict to respect human rights, including with regard to those forcibly displaced, and the principle of non-refoulement,")
   (resq "Reiterating" "the call upon all parties to the armed conflict to comply with their obligations under international humanitarian law regarding the protection of civilians and civilian objects, and the environment, and to spare civilian objects, including those critical to the delivery of essential services to the civilian population, refraining from attacking, destroying, removing or rendering useless objects that are indispensable to the survival of the civilian population, and respecting and protecting humanitarian personnel and consignments used for humanitarian relief operations,")
   (resq "Reaffirming" "the principles of humanity, neutrality, impartiality and independence in the provision of humanitarian assistance, and reaffirming also the need for all actors engaged in the provision of humanitarian assistance in situations of complex emergencies to promote and fully respect these principles,")
   (x-ol (x-li (resq "Reiterates" "the need for the full implementation of resolution ES-11/1 of 2 March 2022, entitled &quot;Aggression against Ukraine&quot;;"))
         (x-li (resq "Demands" "an immediate cessation of the hostilities by the Russian Federation against Ukraine, in particular of any attacks against civilians and civilian objects;"))
         (x-li (resq "Also demands" "that civilians, including humanitarian personnel, journalists and persons in vulnerable situations, including women and children, be fully protected;"))
         (x-li (resq "Further demands" "full respect for and protection of all medical personnel and humanitarian personnel exclusively engaged in medical duties, their means of transport and equipment, as well as hospitals and other medical facilities;"))
         (x-li (resq "Demands" "full respect for and protection of objects indispensable to the survival of the civilian population and civilian infrastructure that is critical to the delivery of essential services in armed conflict;"))
         (x-li (resq "Also demands" "that all parties protect civilians fleeing armed conflict and violence, including foreign nationals, notably students, without discrimination, to allow voluntary, safe and unhindered passage;"))
         (x-li (resq "Further demands" "that the parties comply with their obligation to ensure the safe and unhindered humanitarian access of humanitarian personnel as well as their means of transport, supplies and equipment to those in need in Ukraine and its neighbouring countries;"))
         (x-li (resq "Stresses" "that the sieges of cities in Ukraine, in particular the city of Mariupol, further aggravate the humanitarian situation for the civilian population and hamper evacuation efforts, and therefore demands to put an end to these sieges;"))
         (x-li (resq "Condemns" "all violations of international humanitarian law and violations
and abuses of human rights, and calls upon all parties to the armed conflict to strictly respect international humanitarian law, including the Geneva Conventions of 1949 and Additional Protocol I thereto, of 1977, and to respect international human rights law and international refugee law, including the principle of non-refoulement, as applicable;"))
         (x-li (resq "Calls upon" "Member States to fully fund the United Nations Humanitarian Response Plan 2022, the flash appeal launched by the United Nations for the humanitarian response in Ukraine, as well as the regional refugee response plan for Ukraine and its neighbouring countries, and notes with concern the findings in the Global Humanitarian Overview 2022, including its February 2022 update;"))
         (x-li (resq "Welcomes and urges" "the continued efforts by the Secretary-General, Member States, entities of the United Nations system and the international community to deliver humanitarian assistance as well as assistance and protection for refugees, and also welcomes the appointment by the Secretary-General of a United Nations Crisis Coordinator for Ukraine;"))
         (x-li (resq "Reiterates its request" "to the Emergency Relief Coordinator to provide a report on the humanitarian situation in Ukraine and on the humanitarian response, in accordance with its resolution ES-11/1, and requests the Secretary-General to brief the General Assembly, on a regular basis, on the implementation of the present resolution;"))
         (x-li (resq "Strongly encourages" "the continued negotiations between all parties, and again urges the immediate peaceful resolution of the conflict between the Russian Federation and Ukraine through political dialogue, negotiations, mediation and other peaceful means in accordance with international law;"))
         (x-li (resq "Decides" "to adjourn the eleventh emergency special session of the General Assembly temporarily and to authorize the President of the General Assembly to resume its meetings upon request from Member States.")))))
(a-res-es-11-2)

(defun a-res-es-11-3 ()
  (resolution-template
   "Resolution adopted by the General Assembly on 7 April 2022"
   "ES-11/3. Suspension of the rights of membership of the Russian Federation in the Human Rights Council"
   "un/A_RES_ES-11_3-EN.pdf"
   "A_RES_ES-11_3-EN.html"
   (resq "The General Assembly,")
   (resq "Recalling" "its resolution 60/251 of 15 March 2006, in particular paragraph 8, which states that the General Assembly may suspend the rights of membership in the Human Rights Council of a member of the Council that commits gross and systematic violations of human rights,")
   (resq "Taking note" "of Human Rights Council resolution 49/1 of 4 March 2022, in particular the grave concern of the Council regarding reports of gross and systematic violations and abuses of human rights and violations of international humanitarian law committed by the Russian Federation during its aggression agai nst Ukraine,")
   (resq "Recalling" "its resolutions ES-11/1 of 2 March 2022 and ES-11/2 of 24 March 2022,")
   (resq "Expressing grave concern" "at the ongoing human rights and humanitarian crisis in Ukraine, in particular at the reports of violations and abuses of human rights and violations of international humanitarian law by the Russian Federation, including gross and systematic violations and abuses of human rights, recogniz ing the strong expressions of concern in statements by the Secretary-General and by the United Nations High Commissioner for Human Rights, and noting the latest update on the human rights situation in Ukraine by the human rights monitoring mission in Ukraine, of 26 March 2022,")
   (x-ol (x-li (resq "Decides" "to suspend the rights of membership in the Human Rights Council of the Russian Federation;"))
         (x-li (resq "Also decides" "to review the matter, as appropriate;"))
         (x-li (resq "Further decides" "to adjourn the eleventh emergency special session of the General Assembly temporarily and to authorize the President of the General Assembly to resume its meetings upon request from Member States.")))))
(a-res-es-11-3)

(defun a-res-es-11-4 ()
  (resolution-template
   "Resolution adopted by the General Assembly on 12 October 2022"
   "ES-11/4. Territorial integrity of Ukraine: defending the principles of the Charter of the United Nations"
   "un/A_RES_ES-11_4-EN.pdf"
   "A_RES_ES-11_4-EN.html"
   (resq "The General Assembly,")
   (resq "Recalling" "the obligation of all States under Article 2 of the Charter of the United Nations to refrain in their international relations from the threat or use of force against the territorial integrity or political independence of any State, or in any other manner inconsistent with the purposes of the United Nations, and to settle their international disputes by peaceful means in such a manner that international peace and security and justice are not endangered,")
   (resq "Reaffirming" "the principle of customary international law, as restated in its resolution 2625 (XXV) of 24 October 1970, entitled &quot;Declaration on Principles of International Law concerning Friendly Relations and Cooperation among States in accordance with the Charter of the United Nations&quot;, that no territorial acquisition resulting from the threat or use of force shall be recognized as legal,")
   (resq "Recalling" "its resolutions 68/262 of 27 March 2014, entitled &quot;Territorial integrity of Ukraine&quot;, ES-11/1 of 2 March 2022, entitled &quot;Aggression against Ukraine&quot;, and ES-11/2 of 24 March 2022, entitled &quot;Humanitarian consequences of the aggression against Ukraine&quot;,")
   (resq "Noting" "that the Donetsk, Kherson, Luhansk and Zaporizhzhia regions of Ukraine are areas that, in part, are or have been under the temporary military control of the Russian Federation, as a result of aggression, in violation of the sovereignty, political independence and territorial integrity of Ukraine,")
   (resq "Noting also" "that the decisions of 21 February and 29 September 2022 by the Russian Federation related to the status of the Donetsk, Kherson, Luhansk and Zaporizhzhia regions of Ukraine are a violation of the territorial integrity and sovereignty of Ukraine and inconsistent with the principles of the Charter,")
   (resq "Noting with concern" "that the illegal so-called referendums were organized from 23 to 27 September 2022 in these regions as attempts to modify the internationally recognized borders of Ukraine,")
   (resq "Noting" "the Secretary-General's statement of 29 September 2022 in which he recalled that any annexation of a State’s territory by another State resulting from the threat or use of force is a violation of the principles of the Charter and international law,")
   (x-ol (x-li (resq "Reaffirms its commitment" "to the sovereignty, independence, unity and territorial integrity of Ukraine within its internationally recognized borders, extending to its territorial waters;"))
         (x-li (resq "Condemns" "the organization by the Russian Federation of illegal so-called referendums in regions within the internationally recognized borders of Ukraine and the attempted illegal annexation of the Donetsk, Kherson, Luhansk and Zaporizhzhia regions of Ukraine, following the organization of the above-mentioned referendums;"))
         (x-li (resq "Declares" "that the unlawful actions of the Russian Federation with regard to the illegal so-called referendums held from 23 to 27 September 2022 in parts of the Donetsk, Kherson, Luhansk and Zaporizhzhia regions of Ukraine that, in part, are or have been under the temporary military control of the Russian Federation, and the subsequent attempted illegal annexation of these regions, have no validity under international law and do not form the basis for any alteration of the status of these regions of Ukraine;"))
         (x-li (resq "Calls upon" "all States, international organizations and United Nations specialized agencies not to recognize any alteration by the Russian Federation of the status of any or all of the Donetsk, Kherson, Luhansk or Zaporizhzhia regions of Ukraine, and to refrain from any action or dealing that might be interpreted as recognizing any such altered status;"))
         (x-li (resq "Demands" "that the Russian Federation immediately and unconditionally reverse its decisions of 21 February and 29 September 2022 related to the status of certain areas of the Donetsk, Kherson, Luhansk and Zaporizhzhia regions of Ukraine, as they are a violation of the territorial integrity and sovereignty of Ukraine and inconsistent with the principles of the Charter of the United Nations, and immediately, completely and unconditionally withdraw all of its military forces from the territory of Ukraine within its internationally recognized borders;"))
         (x-li (resq "Welcomes" "the efforts of the United Nations, Member States and humanitarian organizations to respond to the humanitarian and refugee crisis;"))
         (x-li (resq "Welcomes and expresses its strong support" "for the continued efforts by the Secretary-General and Member States, and calls upon Member States and international organizations, including the Organization for Security and Cooperation in Europe and other international and regional organizations, to support the de-escalation of the current situation and a peaceful resolution of the conflict through political dialogue, negotiation, mediation and other peaceful means, with respect for the sovereignty and territorial integrity of Ukraine within its internationally recognized borders and in accordance with the principles of the Charter;"))
         (x-li (resq "Decides" "to adjourn the eleventh emergency special session of the General Assembly temporarily and to authorize the President of the General Assembly to resume its meetings upon request from Member States.")))))
(a-res-es-11-4)

