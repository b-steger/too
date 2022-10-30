;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Charter of the United Nations (public domain)
;;;;  Half-materialized as HTML, the first citizen in ZIM files.
;;;; Authority: https://www.un.org/en/about-us/un-charter/ http://treaties.un.org/doc/Publication/CTC/uncharter.pdf.

(defun chapter (number name &rest body)
  "HTML version of a chapter of the UN Charter."
  (dsl nil
       (x-a :name (format nil "chapter~A" number))
       (x-h3 (format nil "CHAPTER ~@R: ~@:(~A~)" number name))
       body))

;;Authority: #'chapter.
(defun chapterj (number name &rest body)
  "HTML version of a chapter of the Statute of the International Court of Justice."
  (dsl nil
       (x-a :name (format nil "chapterj~A" number))
       (x-h3 (format nil "CHAPTER ~@R: ~@:(~A~)" number name))
       body))

(defun subchapter (name &rest body)
  "HTML version of a subchapter of the UN Charter."
  (dsl nil (x-h3 name) body))

(defun article (number &rest body)
  "HTML version of an article of the UN Charter."
  (dsl nil
       (x-a :name (format nil "article~A" number))
       (x-h5 (format nil "Article ~A" number))
       body))

;;Authority: #'article.
(defun articlej (number &rest body)
  "HTML version of an article of the Statute of the International Court of Justice."
  (dsl nil
       (x-a :name (format nil "articlej~A" number))
       (x-h5 (format nil "Article ~A" number))
       body))

(defgeneric link (structural-type number &optional short? roman? icj?)
  (:method ((structural-type (eql :a)) number &optional short? roman? icj?)
    "Link to an article anchor in the UN Charter."
    (declare (ignore roman? icj?))
    (call-next-method "article" number short? nil))
  (:method ((structural-type (eql :aj)) number &optional short? roman? icj?)
    "Link to an article anchor in the Statute of the International Court of Justice."
    (declare (ignore roman? icj?))
    (call-next-method "articlej" number short? nil T))
  (:method ((structural-type (eql :c)) number &optional short? roman? icj?)
    "Link to an chapter anchor in the UN Charter."
    (declare (ignore roman? icj?))
    (call-next-method "chapter" number short? T))
  (:method ((structural-type (eql :cj)) number &optional short? roman? icj?)
    "Link to an chapter anchor in the Statute of the International Court of Justice."
    (declare (ignore roman? icj?))
    (call-next-method "chapterj" number short? T T))
  (:method ((structural-type T) number &optional short? roman? icj?)
    (x-a :href (dsl nil "#" structural-type number)
         (let ((printed-number (if roman? (format nil "~@R" number) number)))
           (if short?
               printed-number
               (format nil "~:(~A~) ~A" (if icj? (subseq structural-type 0 (1- (length structural-type))) structural-type) printed-number)))))
  (:documentation "Link to a structural element anchored in the UN Charter.
                   ROMAN? ICJ? are internal."))

(defun lq (&rest body)
  "Quick helper which wraps BODY in a #'x-li. To be used in tandem with #'olq."
  (list (x-li body)))

(defun olq (&rest body)
  "Quick helper which renders to an ordered list in HTML.
   Elements in BODY may be strings (that will get wrapped in a #'x-li) or lists (that will get spliced in)."
  (x-ol (loop for item in body
              append (if (stringp item)
                         (list (x-li item))
                         item))))
(olq "abc" (lq "def" (link :a 1) "ghi") "jkl")

(defun olabcq (&rest body)
  "Quick helper which produces an ordered list that is enumerated with lowercase alphabetical letters.
   Behaviour of BODY according to the rules of #'olq."
  (x-ol :style "list-style-type:lower-alpha;"
        (loop for item in body
              append (if (stringp item)
                         (list (x-li item))
                         item))))

(defun uncharter ()
  "The Charter of the United Nations (UN, 1945)."
  (flet ((invisible-list (&rest body)
           (x-ul (loop for item in body
                       collect (x-li :style "list-style:none;padding-top:0.4em;padding-bottom:0.25em"
                                     item)))))
    (dir-entry
     :url "uncharter.html"
     :title "Charter of the United Nations"
     :mimetype-id :html
     :content
     (x-html
      (x-head (x-title "Charter of the United Nations")
              "<meta charset='utf8'>")
      (x-body
       (x-h1 :style "text-align:center" "CHARTER OF THE UNITED NATIONS")
       (x-h2 :style "text-align:center" "AND STATUE OF THE INTERNATIONAL COURT OF JUSTICE")
       (x-p :style "text-align:center" "SAN FRANCISCO · 1945")
       (x-hr)
       (x-h2 "CHARTER OF THE UNITED NATIONS")
       (x-p "WE THE PEOPLES OF THE UNITED NATIONS DETERMINED"
            (invisible-list "to save succeeding generations from the scourge of war, which twice in our lifetime has brought untold sorrow to mankind, and"
                            "to reaffirm faith in fundamental human rights, in the dignity and worth of the human person, in the equal rights of men and women and of nations large and small, and"
                            "to establish conditions under which justice and respect for the obligations arising from treaties and other sources of international law can be maintained, and"
                            "to promote social progress and better standards of life in larger freedom,"))
       (x-p "AND FOR THESE ENDS"
            (invisible-list "to practice tolerance and live together in peace with one another as good neighbors, and"
                            "to unite our strength to maintain international peace and security, and"
                            "to ensure, by the acceptance of principles and the institution of methods, that armed force shall not be used, save in the common interest, and"
                            "to employ international machinery for the promotion of the economic and social advancement of all peoples,"))
       (x-p "HAVE RESOLVED TO COMBINE OUR EFFORTS TO ACCOMPLISH THESE AIMS.")
       (x-p "Accordingly, our respective Governments, through representatives assembled in the city of San Francisco, who have exhibited their full powers found to be in good and due form, have agreed to the present Charter of the United Nations and do hereby establish an international organization to be known as the United Nations.")
       (chapter 1 "Purposes and Principles"
                (article 1
                         "The Purposes of the United Nations are:"
                         (olq "To maintain international peace and security, and to that end: to take effective collective measures for the prevention and removal of threats to the peace, and for the suppression of acts of aggression or other breaches of the peace, and to bring about by peaceful means, and in conformity with the principles of justice and international law, adjustment or settlement of international disputes or situations which might lead to a breach of the peace;"
                              "To develop friendly relations among nations based on respect for the principle of equal rights and self-determination of peoples, and to take other appropriate measures to strengthen universal peace;"
                              "To achieve international cooperation in solving international problems of an economic, social, cultural, or humanitarian character, and in promoting and encouraging respect for human rights and for fundamental freedoms for all without distinction as to race, sex, language, or religion; and"
                              "To be a center for harmonizing the actions of nations in the attainment of these common ends."))
                (article 2
                         "The Organization and its Members, in pursuit of the Purposes stated in " (link :a 1) ", shall act in accordance with the following Principles."
                         (olq "The Organization is based on the principle of the sovereign equality of all its Members."
                              "All Members, in order to ensure to all of them the rights and benefits resulting from membership, shall fulfil in good faith the obligations assumed by them in accordance with the present Charter."
                              "All Members shall settle their international disputes by peaceful means in such a manner that international peace and security, and justice, are not endangered."
                              "All Members shall refrain in their international relations from the threat or use of force against the territorial integrity or political independence of any state, or in any other manner inconsistent with the Purposes of the United Nations."
                              "All Members shall give the United Nations every assistance in any action it takes in accordance with the present Charter, and shall refrain from giving assistance to any state against which the United Nations is taking preventive or enforcement action."
                              "The Organization shall ensure that states which are not Members of the United Nations act in accordance with these Principles so far as may be necessary for the maintenance of international peace and security."
                              (lq "Nothing contained in the present Charter shall authorize the United Nations to intervene in matters which are essentially within the domestic jurisdiction of any state or shall require the Members to submit such matters to settlement under the present Charter; but this principle shall not prejudice the application of enforcement measures under " (link :c 7) "."))))
       (chapter 2 "Membership"
                (article 3 "The original Members of the United Nations shall be the states which, having participated in the United Nations Conference on International Organization at San Francisco, or having previously signed the Declaration by United Nations of January 1,1942, sign the present Charter and ratify it in accordance with " (link :a 110) ".")
                (article 4 (olq "Membership in the United Nations is open to all other peace-loving states which accept the obligations contained in the present Charter and, in the judgment of the Organization, are able and willing to carry out these obligations."
                                "The admission of any such state to membership in the United Nations will be effected by a decision of the General Assembly upon the recommendation of the Security Council."))
                (article 5 "A Member of the United Nations against which preventive or enforcement action has been taken by the Security Council may be suspended from the exercise of the rights and privileges of membership by the General Assembly upon the recommendation of the Security Council. The exercise of these rights and privileges may be restored by the Security Council.")
                (article 6 "A Member of the United Nations which has persistently violated the Principles contained in the present Charter may be expelled from the Organization by the General Assembly upon the recommendation of the Security Council."))
       (chapter 3 "Organs"
                (article 7 (olq "There are established as the principal organs of the United Nations: a General Assembly, a Security Council, an Economic and Social Council, a Trusteeship Council, an International Court of Justice, and a Secretariat."
                                "Such subsidiary organs as may be found necessary may be established in accordance with the present Charter."))
                (article 8 "The United Nations shall place no restrictions on the eligibility of men and women to participate in any capacity and under conditions of equality in its principal and subsidiary organs."))
       (chapter 4 "The General Assembly"
                (subchapter "Composition"
                            (article 9 (olq "The General Assembly shall consist of all the Members of the United Nations."
                                            "Each Member shall have not more than five representatives in the General Assembly.")))
                (subchapter "Functions and Powers"
                            (article 10 "The General Assembly may discuss any questions or any matters within the scope of the present Charter or relating to the powers and functions of any organs provided for in the present Charter, and, except as provided in " (link :a 12) ", may make recommendations to the Members of the United Nations or to the Security Council or to both on any such questions or matters.")
                            (article 11 (olq "The General Assembly may consider the general principles of cooperation in the maintenance of international peace and security, including the principles governing disarmament and the regulation of armaments, and may make recommendations with regard to such principles to the Members or to the Security Council or to both."
                                             (lq "The General Assembly may discuss any questions relating to the maintenance of international peace and security brought before it by any Member of the United Nations, or by the Security Council, or by a state which is not a Member of the United Nations in accordance with " (link :a 35) ", paragraph 2, and, except as provided in " (link :a 12) ", may make recommendations with regard to any such questions to the state or states concerned or to the Security Council or to both. Any such question on which action is necessary shall be referred to the Security Council by the General Assembly either before or after discussion.")
                                             "The General Assembly may call the attention of the Security Council to situations which are likely to endanger international peace and security."
                                             (lq "The powers of the General Assembly set forth in this Article shall not limit the general scope of " (link :a 10) ".")))
                            (article 12 (olq "While the Security Council is exercising in respect of any dispute or situation the functions assigned to it in the present Charter, the General Assembly shall not make any recommendation with regard to that dispute or situation unless the Security Council so requests."
                                             "The Secretary-General, with the consent of the Security Council, shall notify the General Assembly at each session of any matters relative to the maintenance of international peace and security which are being dealt with by the Security Council and shall similarly notify the General Assembly, or the Members of the United Nations if the General Assembly is not in session, immediately the Security Council ceases to deal with such matters."))
                            (article 13 (olq (lq "The General Assembly shall initiate studies and make recommendations for the purpose of:"
                                                 (olabcq "promoting international cooperation in the political field and encouraging the progressive development of international law and its codification;"
                                                         "promoting international cooperation in the economic, social, cultural, educational, and health fields, and assisting in the realization of human rights and fundamental freedoms for all without distinction as to race, sex, language, or religion."))
                                             (lq "The further responsibilities, functions, and powers of the General Assembly with respect to matters mentioned in paragraph 1(b) above are set forth in Chapters " (link :c 9 T) " and " (link :c 10 T) ".")))
                            (article 14 "Subject to the provisions of " (link :a 12) ", the General Assembly may recommend measures for the peaceful adjustment of any situation, regardless of origin, which it deems likely to impair the general welfare or friendly relations among nations, including situations resulting from a violation of the provisions of the present Charter setting forth the Purposes and Principles of the United Nations.")
                            (article 15 (olq "The General Assembly shall receive and consider annual and special reports from the Security Council; these reports shall include an account of the measures that the Security Council has decided upon or taken to maintain international peace and security."
                                             "The General Assembly shall receive and consider reports from the other organs of the United Nations."))
                            (article 16 "The General Assembly shall perform such functions with respect to the international trusteeship system as are assigned to it under Chapters " (link :c 7 T) " and " (link :c 8 T) ", including the approval of the trusteeship agreements for areas not designated as strategic.")
                            (article 17 (olq "The General Assembly shall consider and approve the budget of the Organization."
                                             "The expenses of the Organization shall be borne by the Members as apportioned by the General Assembly."
                                             (lq "The General Assembly shall consider and approve any financial and budgetary arrangements with specialized agencies referred to in " (link :a 57) " and shall examine the administrative budgets of such specialized agencies with a view to making recommendations to the agencies concerned."))))
                (subchapter "Voting"
                            (article 18 (olq "Each member of the General Assembly shall have one vote."
                                             (lq "Decisions of the General Assembly on important questions shall be made by a two-thirds majority of the members present and voting. These questions shall include: recommendations with respect to the maintenance of international peace and security, the election of the non-permanent members of the Security Council, the election of the members of the Economic and Social Council, the election of members of the Trusteeship Council in accordance with paragraph 1(c) of " (link :a 86) ", the admission of new Members to the United Nations, the suspension of the rights and privileges of membership, the expulsion of Members, questions relating to the operation of the trusteeship system, and budgetary questions.")
                                             "Decisions on other questions, including the determination of additional categories of questions to be decided by a two-thirds majority, shall be made by a majority of the members present and voting."))
                            (article 19 "A Member of the United Nations which is in arrears in the payment of its financial contributions to the Organization shall have no vote in the General Assembly if the amount of its arrears equals or exceeds the amount of the contributions due from it for the preceding two full years. The General Assembly may, nevertheless, permit such a Member to vote if it is satisfied that the failure to pay is due to conditions beyond the control of the Member. "))
                (subchapter "Procedure"
                            (article 20 "The General Assembly shall meet in regular annual sessions and in such special sessions as occasion may require. Special sessions shall be convoked by the Secretary-General at the request of the Security Council or of a majority of the Members of the United Nations.")
                            (article 21 "The General Assembly shall adopt its own rules of procedure. It shall elect its President for each session.")
                            (article 22 "The General Assembly may establish such subsidiary organs as it deems necessary for the performance of its functions.")))
       (chapter 5 "The Security Council"
                (subchapter "Composition"
                            (article 23 (olq "The Security Council shall consist of eleven Members of the United Nations. The Republic of China, France, the Union of Soviet Socialist Republics, the United Kingdom of Great Britain and Northern Ireland, and the United States of America shall be permanent members of the Security Council. The General Assembly shall elect six other Members of the United Nations to be non-permanent members of the Security Council, due regard being specially paid, in the first instance to the contribution of Members of the United Nations to the maintenance of international peace and security and to the other purposes of the Organization, and also to equitable geographical distribution."
                                             "The non-permanent members of the Security Council shall be elected for a term of two years. In the first election of the non-permanent members, however, three shall be chosen for a term of one year. A retiring member shall not be eligible for immediate re-election."
                                             "Each member of the Security Council shall have one representative.")))
                (subchapter "Functions and Powers"
                            (article 24 (olq "In order to ensure prompt and effective action by the United Nations, its Members confer on the Security Council primary responsibility for the maintenance of international peace and security, and agree that in carrying out its duties under this responsibility the Security Council acts on their behalf."
                                             (lq "In discharging these duties the Security Council shall act in accordance with the Purposes and Principles of the United Nations. The specific powers granted to the Security Council for the discharge of these duties are laid down in Chapters " (link :c 6 T) ", " (link :c 7 T) ", " (link :c 8 T) ", and " (link :c 12 T) ".")
                                             "The Security Council shall submit annual and, when necessary, special reports to the General Assembly for its consideration."))
                            (article 25 "The Members of the United Nations agree to accept and carry out the decisions of the Security Council in accordance with the present Charter.")
                            (article 26 "In order to promote the establishment and maintenance of international peace and security with the least diversion for armaments of the world's human and economic resources, the Security Council shall be responsible for formulating, with the assistance of the Military Staff Committee referred to in " (link :a 47) ", plans to be submitted to the Members of the United Nations for the establishment of a system for the regulation of armaments."))
                (subchapter "Voting"
                            (article 27 (olq "Each member of the Security Council shall have one vote."
                                             "Decisions of the Security Council on procedural matters shall be made by an affirmative vote of seven members."
                                             (lq "Decisions of the Security Council on all other matters shall be made by an affirmative vote of seven members including the concurring votes of the permanent members; provided that, in decisions under Chapter " (link :c 6 T) ", and under paragraph 3 of " (link :a 52) ", a party to a dispute shall abstain from voting."))))
                (subchapter "Procedure"
                            (article 28 (olq "The Security Council shall be so organized as to be able to function continuously. Each member of the Security Council shall for this purpose be represented at all times at the seat of the Organization."
                                             "The Security Council shall hold periodic meetings at which each of its members may, if it so desires, be represented by a member of the government or by some other specially designated representative."
                                             "The Security Council may hold meetings at such places other than the seat of the Organization as in its judgment will best facilitate its work."))
                            (article 29 "The Security Council may establish such subsidiary organs as it deems necessary for the performance of its functions.")
                            (article 30 "The Security Council shall adopt its own rules of procedure, including the method of selecting its President.")
                            (article 31 "Any Member of the United Nations which is not a member of the Security Council may participate, without vote, in the discussion of any question brought before the Security Council whenever the latter considers that the interests of that Member are specially affected.")
                            (article 32 "Any Member of the United Nations which is not a member of the Security Council or any state which is not a Member of the United Nations, if it is a party to a dispute under consideration by the Security Council, shall be invited to participate, without vote, in the discussion relating to the dispute. The Security Council shall lay down such conditions as it deems just for the participation, of a state which is not a Member of the United Nations.")))
       (chapter 6 "Pacific settlement of disputes"
                (article 33 (olq "The parties to any dispute, the continuance of which is likely to endanger the maintenance of international peace and security, shall, first of all, seek a solution by negotiation, enquiry, mediation, conciliation, arbitration, judicial settlement, resort to regional agencies or arrangements, or other peaceful means of their own choice."
                                 "The Security Council shall, when it deems necessary, call upon the parties to settle their dispute by such means."))
                (article 34 "The Security Council may investigate any dispute, or any situation which might lead to international friction or give rise to a dispute, in order to determine whether the continuance of the dispute or situation is likely to endanger the maintenance of international peace and security.")
                (article 35 (olq (lq "Any Member of the United Nations may bring any dispute, or any situation of the nature referred to in " (link :a 34) ", to the attention of the Security Council or of the General Assembly.")
                                 "A state which is not a Member of the United Nations may bring to the attention of the Security Council or of the General Assembly any dispute to which it is a party if it accepts in advance, for the purposes of the dispute, the obligations of pacific settlement provided in the present Charter."
                                 (lq "The proceedings of the General Assembly in respect of matters brought to its attention under this Article will be subject to the provisions of Articles " (link :a 11 T) " and " (link :a 12 T) ".")))
                (article 36 (olq (lq "The Security Council may, at any stage of a dispute of the nature referred to in " (link :a 33) " or of a situation of like nature, recommend appropriate procedures or methods of adjustment.")
                                 "The Security Council should take into consideration any procedures for the settlement of the dispute which have already been adopted by the parties."
                                 "In making recommendations under this Article the Security Council should also take into consideration that legal disputes should as a general rule be referred by the parties to the International Court of Justice in accordance with the provisions of the Statute of the Court."))
                (article 37 (olq (lq "Should the parties to a dispute of the nature referred to in " (link :a 33) " fail to settle it by the means indicated in that Article, they shall refer it to the Security Council.")
                                 (lq "If the Security Council deems that the continuance of the dispute is in fact likely to endanger the maintenance of international peace and security, it shall decide whether to take action under " (link :a 36) " or to recommend such terms of settlement as it may consider appropriate.")))
                (article 38 "Without prejudice to the provisions of Articles " (link :a 33 T) " to " (link :a 37 T) ", the Security Council may, if all the parties to any dispute so request, make recommendations to the parties with a view to a pacific settlement of the dispute."))
       (chapter 7 "Action with respect to threats to the peace, breaches of the peace and acts of aggression"
                (article 39 "The Security Council shall determine the existence of any threat to the peace, breach of the peace, or act of aggression and shall make recommendations, or decide what measures shall be taken in accordance with Articles " (link :a 41 T) " and " (link :a 42 T) ", to maintain or restore international peace and security.")
                (article 40 "In order to prevent an aggravation of the situation, the Security Council may, before making the recommendations or deciding upon the measures provided for in " (link :a 39) ", call upon the parties concerned to comply with such provisional measures as it deems necessary or desirable. Such provisional measures shall be without prejudice to the rights, claims, or position of the parties concerned. The Security Council shall duly take account of failure to comply with such provisional measures.")
                (article 41 "The Security Council may decide what measures not involving the use of armed force are to be employed to give effect to its decisions, and it may call upon the Members of the United Nations to apply such measures. These may include complete or partial interruption of economic relations and of rail, sea, air, postal, telegraphic, radio, and other means of communication, and the severance of diplomatic relations.")
                (article 42 "Should the Security Council consider that measures provided for in " (link :a 41) " would be inadequate or have proved to be inadequate, it may take such action by air, sea, or land forces as may be necessary to maintain or restore international peace and security. Such action may include demonstrations, blockade, and other operations by air, sea, or land forces of Members of the United Nations.")
                (article 43 (olq "All Members of the United Nations, in order to contribute to the maintenance of international peace and security, undertake to make available to the Security Council, on its call and in accordance with a special agreement or agreements, armed forces, assistance, and facilities, including rights of passage, necessary for the purpose of maintaining international peace and security."
                                 "Such agreement or agreements shall govern the numbers and types of forces, their degree of readiness and general location, and the nature of the facilities and assistance to be provided."
                                 "The agreement or agreements shall be negotiated as soon as possible on the initiative of the Security Council. They shall be concluded between the Security Council and Members or between the Security Council and groups of Members and shall be subject to ratification by the signatory states in accordance with their respective constitutional processes."))
                (article 44 "When the Security Council has decided to use force it shall, before calling upon a Member not ​represented on it to provide armed forces in fulfillment of the obligations assumed under " (link :a 43) ", invite that Member, if the Member so desires, to participate in the decisions of the Security Council concerning the employment of contingents of that Member's armed forces.")
                (article 45 "In order to enable the United Nations to take urgent military measures, Members shall hold immediately available national air-force contingents for combined international enforcement action. The strength and degree of readiness of these contingents and plans for their combined action shall be determined, within the limits laid down in the special agreement or agreements referred to in " (link :a 43) ", by the Security Council with the assistance of the Military Staff Committee.")
                (article 46 "Plans for the application of armed force shall be made by the Security Council with the assistance of the Military Staff Committee.")
                (article 47 (olq "There shall be established a Military Staff Committee to advise and assist the Security Council on all questions relating to the Security Council's military requirements for the maintenance of international peace and security, the employment and command of forces placed at its disposal, the regulation of armaments, and possible disarmament."
                                 "The Military Staff Committee shall consist of the Chiefs of Staff of the permanent members of the Security Council or their representatives. Any Member of the United Nations not permanently represented on the Committee shall be invited by the Committee to be associated with it when the efficient discharge of the Committee's responsibilities requires the participation of that Member in its work."
                                 "The Military Staff Committee shall be responsible under the Security Council for the strategic direction of any armed forces placed at the disposal of the Security Council. Questions relating to the command of such forces shall be worked out subsequently."
                                 "The Military Staff Committee, with the authorization of the Security Council and after consultation with appropriate regional agencies, may establish regional subcommittees."))
                (article 48 (olq "The action required to carry out the decisions of the Security Council for the maintenance of international peace and security shall be taken by all the Members of the United Nations or by some of them, as the Security Council may determine."
                                 "Such decisions shall be carried out by the Members of the United Nations directly and through their action in the appropriate international agencies of which they are members."))
                (article 49 "The Members of the United Nations shall join in affording mutual assistance in carrying out the measures decided upon by the Security Council.")
                (article 50 "If preventive or enforcement measures against any state are taken by the Security Council, any other state, whether a Member of the United Nations or not, which finds itself confronted with special economic problems arising from the carrying out of those measures shall have the right to consult the Security Council with regard to a solution of those problems.")
                (article 51 "Nothing in the present Charter shall impair the inherent right of individual or collective self-defense if an armed attack occurs against a Member of the United Nations, until the Security Council has taken the measures necessary to maintain international peace and security. Measures taken by Members in the exercise of this right of self-defense shall be immediately reported to the Security Council and shall not in any way affect the authority and responsibility of the Security Council under the present Charter to take at any time such action as it deems necessary in order to maintain or restore international peace and security."))
       (chapter 8 "Regional arrangements"
                (article 52 (olq "Nothing in the present Charter precludes the existence of regional arrangements or agencies for dealing with such matters relating to the maintenance of international peace and security as are appropriate for regional action, provided that such arrangements or agencies and their activities are consistent with the Purposes and Principles of the United Nations."
                                 "The Members of the United Nations entering into such arrangements or constituting such agencies shall make every effort to achieve pacific settlement of local disputes through such regional arrangements or by such regional agencies before referring them to the Security Council."
                                 "The Security Council shall encourage the development of pacific settlement of local disputes through such regional arrangements or by such regional agencies either on the initiative of the states concerned or by reference from the Security Council."
                                 (lq "This Article in no way impairs the application of Articles " (link :a 34 T) " and " (link :a 35 T) ".")))
                (article 53 (olq (lq "The Security Council shall, where appropriate, utilize such regional arrangements or agencies for enforcement action Under its authority. But no enforcement action shall be taken under regional arrangements or by regional agencies without the authorization of the Security Council, with the exception of measures against any enemy state, as defined in paragraph 2 of this Article, provided for pursuant to " (link :a 107) " or in regional arrangements directed against renewal of aggressive policy on the part of any such state, until such time as the Organization may, on request of the Governments concerned, be charged with the responsibility for preventing further aggression by such a state.")
                                 "The term enemy state as used in paragraph 1 of this Article applies to any state which during the Second World War has been an enemy of any signatory of the present Charter."))
                (article 54 "The Security Council shall at all times be kept fully informed of activities undertaken or in contemplation under regional arrangements or by regional agencies for the maintenance of international peace and security."))
       (chapter 9 "International economic and social cooperation"
                (article 55 "With a view to the creation of conditions of stability and well-being which are necessary for peaceful and friendly relations among nations based on respect for the principle of equal rights and self-determination of peoples, the United Nations shall promote:"
                         (olabcq "higher standards of living, full employment, and conditions of economic and social progress and development;"
                                 "solutions of international economic, social, health, and related problems; and international cultural and educational cooperation; and"
                                 "universal respect for, and observance of, human rights and fundamental freedoms for all without distinction as to race, sex, language, or religion."))
                (article 56 "All Members pledge themselves to take joint and separate action in cooperation with the Organization for the achievement of the purposes set forth in " (link :a 55) ".")
                (article 57 (olq (lq "The various specialized agencies, established by intergovernmental agreement and having wide international responsibilities, as defined in their basic instruments, in economic, social, cultural, educational, health, and related fields, shall be brought into relationship with the United Nations in accordance with the provisions of " (link :a 63) ".")
                                 "Such agencies thus brought into relationship with the United Nations are hereinafter referred to as specialized agencies."))
                (article 58 "The Organization shall make recommendations for the coordination of the policies and activities of the specialized agencies.")
                (article 59 "The Organization shall, where appropriate, initiate negotiations among the states concerned for the creation of any new specialized agencies required for the accomplishment of the purposes set forth in " (link :a 55) ".")
                (article 60 "Responsibility for the discharge of the functions of the Organization set forth in this Chapter shall be vested in the General Assembly and, under the authority of the General Assembly, in the Economic and Social Council, which shall have for this purpose the powers set forth in " (link :c 10) "."))
       (chapter 10 "The economic and social council"
                (subchapter "Composition"
                            (article 61
                                     "The Economic and Social Council shall consist of eighteen Members of the United Nations elected by the General Assembly."
                                     "Subject to the provisions of paragraph 3, six members of the Economic and Social Council shall be elected each year for a term of three years. A retiring member shall be eligible for immediate re-election."
                                     "At the first election, eighteen members of the Economic and Social Council shall be chosen. The term of office of six members so chosen shall expire at the end of one year, and of six other members at the end of two years, in accordance with arrangements made by the General Assembly."
                                     "Each member of the Economic and Social Council shall have one representative."))
                (subchapter "Functions and Powers"
                            (article 62 (olq "The Economic and Social Council may make or initiate studies and reports with respect to international economic, social, cultural, educational, health, and related matters and may make recommendations with respect to any such matters to the General Assembly, to the Members of the United Nations, and to the specialized agencies concerned."
                                             "It may make recommendations for the purpose of promoting respect for, and observance of, human rights and fundamental freedoms for all."
                                             "It may prepare draft conventions for submission to the General Assembly, with respect to matters falling within its competence."
                                             "It may call, in accordance with the rules prescribed by the United Nations, international conferences on matters falling within its competence."))
                            (article 63 (olq (lq "The Economic and Social Council may enter into agreements with any of the agencies referred to in " (link :a 57) ", defining the terms on which the agency concerned shall be brought into relationship with the United Nations. Such agreements shall be subject to approval by the General Assembly.")
                                             "It may coordinate the activities of the specialized agencies through consultation with and recommendations to such agencies and through recommendations to the General Assembly and to the Members of the United Nations."))
                            (article 64 (olq "The Economic and Social Council may take appropriate steps to obtain regular reports from the specialized agencies. It may make arrangements with the Members of the United Nations and with the specialized agencies to obtain reports on the steps taken to give effect to its own recommendations and to recommendations on matters falling within its competence made by the General Assembly."
                                             "It may communicate its observations on these reports to the General Assembly."))
                            (article 65 "The Economic and Social Council may furnish information to the Security Council and shall assist the Security Council upon its request.")
                            (article 66 (olq "The Economic and Social Council shall perform such functions as fall within its competence in connection with the carrying out of the recommendations of the General Assembly."
                                             "It may, with the approval of the General Assembly, perform services at the request of Members of the United Nations and at the request of specialized agencies."
                                             "It shall perform such other functions as are specified elsewhere in the present Charter or as may be assigned to it by the General Assembly.")))
                (subchapter "Voting"
                            (article 67 (olq "Each member of the Economic and Social Council shall have one vote."
                                             "Decisions of the Economic and Social Council shall be made by a majority of the members present and voting.")))
                (subchapter "Procedure"
                            (article 68 "The Economic and Social Council shall set up commissions in economic and social fields and for the promotion of human rights, and such other commissions as may be required for the performance of its functions.")
                            (article 69 "The Economic and Social Council shall invite any Member of the United Nations to participate, without vote, in its deliberations on any matter of particular concern to that Member.")
                            (article 70 "The Economic and Social Council may make arrangements for representatives of the specialized agencies to participate, without vote, in its deliberations and in those of the commissions established by it, and for its representatives to participate in the deliberations of the specialized agencies.")
                            (article 71 "The Economic and Social Council may make suitable arrangements for consultation with nongovernmental organizations which are concerned with matters within its competence. Such ​arrangements may be made with international organizations and, where appropriate, with national organizations after consultation with the Member of the United Nations concerned.")
                            (article 72 (olq "The Economic and Social Council shall adopt its own rules of procedure, including the method of selecting its President."
                                             "The Economic and Social Council shall meet as required in accordance with its rules, which shall include provision for the convening of meetings on the request of a majority of its members."))))
       (chapter 11 "Declaration regarding non-self-governing territories"
                (article 73
                         "Members of the United Nations which have or assume responsibilities for the administration of territories whose peoples have not yet attained a full measure of self-government recognize the principle that the interests of the inhabitants of these territories are paramount, and accept as a sacred trust the obligation to promote to the utmost, within the system of international peace and security established by the present Charter, the well-being of the inhabitants of these territories, and, to this end:"
                         (olabcq "to ensure, with due respect for the culture of the peoples concerned, their political, economic, social, and educational advancement, their just treatment, and their protection against abuses;"
                                 "to develop self-government, to take due account of the political aspirations of the peoples, and to assist them in the progressive development of their free political institutions, according to the particular circumstances of each territory and its peoples and their varying stages of advancement;"
                                 "to further international peace and security;"
                                 "to promote constructive measures of development, to encourage research, and to cooperate with one another and, when and where appropriate, with specialized international bodies with a view to the practical achievement of the social, economic, and scientific purposes set forth in this Article; and"
                                 (lq "to transmit regularly to the Secretary General for information purposes, subject to such limitation as security and constitutional considerations may require, statistical and other information of a technical nature relating to economic, social, and educational conditions in the territories for which they are respectively responsible other than those territories to which Chapters " (link :c 12 T) " and " (link :c 13 T) " apply.")))
                (article 74 "Members of the United Nations also agree that their policy in respect of the territories to which this Chapter applies, no less than in respect of their metropolitan areas, must be based on the general principle of good-neighborliness, due account being taken of the interests and well-being of the rest of the world, in social, economic, and commercial matters."))
       (chapter 12 "International trusteeship system"
                (article 75 "The United Nations shall establish under its authority an international trusteeship system for the administration and supervision of such territories as may be placed thereunder by subsequent individual agreements. These territories are hereinafter referred to as trust territories.")
                (article 76
                         "The basic objectives of the trusteeship system, ​in accordance with the Purposes of the United Nations laid down in " (link :a 1) " of the present Charter, shall be:"
                         (olabcq "to further international peace and security;"
                                 "to promote the political, economic, social, and educational advancement of the inhabitants of the trust territories, and their progressive development towards self-government or independence as may be appropriate to the particular circumstances of each territory and its peoples and the freely expressed wishes of the peoples concerned, and as may be provided by the terms of each trusteeship agreement;"
                                 "to encourage respect for human rights and for fundamental freedoms for all without distinction as to race, sex, language, or religion, and to encourage recognition of the interdependence of the peoples of the world; and"
                                 (lq "to ensure equal treatment in social, economic, and commercial matters for all Members of the United Nations and their nationals, and also equal treatment for the latter in the administration of justice, without prejudice to the attainment of the foregoing objectives and subject to the provisions of " (link :a 80) ".")))
                (article 77 (olq (lq "The trusteeship system shall apply to such territories in the following categories as may be placed thereunder by means of trusteeship agreements:"
                                     (olabcq "territories now held under mandate;"
                                             "territories which may be detached from enemy states as a result of the Second World War; and"
                                             "territories voluntarily placed under the system by states responsible for their administration."))
                                 "It will be a matter for subsequent agreement as to which territories in the foregoing categories will be brought under the trusteeship system and upon what terms."))
                (article 78 "The trusteeship system shall not apply to territories which have become Members of the United Nations, relationship among which shall be based on respect for the principle of sovereign equality.")
                (article 79 "The terms of trusteeship for each territory to be placed under the trusteeship system, including any alteration or amendment, shall be agreed upon by the states directly concerned, including the mandatory power in the case of territories held under mandate by a Member of the United Nations, and shall be approved as provided for in Articles " (link :a 83 T) " and " (link :a 85 T) ".")
                (article 80 (olq (lq "Except as may be agreed upon in individual trusteeship agreements, made under Articles " (link :a 77 T) ", " (link :a 79 T) ", and " (link :a 81 T) ", placing each territory under the trusteeship system, and until such agreements have been concluded, nothing in this Chapter shall be construed in or of itself to alter in any manner the rights whatsoever of any states or any peoples or the terms of existing international instruments to which Members of the United Nations may respectively be parties.")
                                 (lq "Paragraph 1 of this Article shall not be interpreted as giving grounds for delay or postponement of the negotiation and conclusion of agreements for placing mandated and other territories under the trusteeship system as provided for in " (link :a 77) ".")))
                (article 81 "The trusteeship agreement shall in each case include the terms under which the trust territory will be administered and designate the authority which will exercise the administration of the trust territory. Such authority, hereinafter called the administering authority, may be one or more states or the Organization itself.")
                (article 82 "There may be designated, in any trusteeship agreement, a strategic area or areas which may include part or all of the trust territory to which the agreement applies, without prejudice to any special agreement or agreements made under " (link :a 43) ".")
                (article 83 (olq "All functions of the United Nations relating to strategic areas, including the approval of the terms of the trusteeship agreements and of their alteration or amendment, shall be exercised by the Security Council."
                                 (lq "The basic objectives set forth in " (link :a 76) " shall be applicable to the people of each strategic area.")
                                 "The Security Council shall, subject to the provisions of the trusteeship agreements and without prejudice to security considerations, avail itself of the assistance of the Trusteeship Council to perform those functions of the United Nations under the trusteeship system relating to political, economic, social, and educational matters in the strategic areas."))
                (article 84 "It shall be the duty of the administering authority to ensure that the trust territory shall play its part in the maintenance of international peace and security. To this end the administering authority may make use of volunteer forces, facilities, and assistance from the trust territory in carrying out the obligations towards the Security Council undertaken in this regard by the administering authority, as well as for local defense and the maintenance of law and order within the trust territory.")
                (article 85 (olq "The functions of the United Nations with regard to trusteeship agreements for all areas not designated as strategic, including the approval of the terms of the trusteeship agreements and of their alteration or amendment, shall be exercised by the General Assembly."
                                 "The Trusteeship Council, operating under the authority of the General Assembly, shall assist the General Assembly in carrying out these functions.")))
       (chapter 13 "The trusteeship council"
                (subchapter "Composition"
                            (article 86 (olq (lq "The Trusteeship Council shall consist of the following Members of the United Nations:"
                                                 (olabcq "those Members administering trust territories;"
                                                         (lq "such of those Members mentioned by name in " (link :a 23) " as are not administering trust territories; and")
                                                         "as many other Members elected for threeyear terms by the General Assembly as may be necessary to ensure that the total number of members of the Trusteeship Council is equally divided between those Members of the United Nations which administer trust territories and those which do not."))
                                             "Each member of the Trusteeship Council shall designate one specially qualified person to represent it therein.")))
                (subchapter "Functions and Powers"
                            (article 87
                                     "The General Assembly and, under its authority, the Trusteeship Council, in carrying out their functions, may:"
                                     (olabcq "consider reports submitted by the administering authority;"
                                             "accept petitions and examine them in consultation with the administering authority;"
                                             "provide for periodic visits to the respective trust territories at times agreed upon with the administering authority; and"
                                             "take these and other actions in conformity with the terms of the trusteeship agreements."))
                            (article 88 "The Trusteeship Council shall formulate a questionnaire on the political, economic, social, and educational advancement of the inhabitants of each trust territory, and the administering authority for each trust territory within the competence of the General Assembly shall make an annual report to the General Assembly upon the basis of such questionnaire."))
                (subchapter "Voting"
                            (article 89 (olq "Each member of the Trusteeship Council shall have one vote."
                                             "Decisions of the Trusteeship Council shall be made by a majority of the members present and voting.")))
                (subchapter "Procedure"
                            (article 90 (olq "The Trusteeship Council shall adopt its own rules of procedure, including the method of selecting its President."
                                             "The Trusteeship Council shall meet as required in accordance with its rules, which shall include provision for the convening of meetings on the request of a majority of its members."))
                            (article 91 "The Trusteeship Council shall, when appropriate, avail itself of the assistance of the Economic and Social Council and of the specialized agencies in regard to matters with which they are respectively concerned.")))
       (chapter 14 "The International Court of Justice"
                (article 92 "The International Court of Justice shall be the principal judicial organ of the United Nations. It shall function in accordance with the annexed Statute, which is based upon the Statute of the Permanent Court of International Justice and forms an integral part of the present Charter.")
                (article 93 (olq "All Members of the United Nations are <i>ipso facto</i> parties to the Statute of the International Court of Justice."
                                 "A state which is not a Member of the United Nations may become a party to the Statute of the International Court of Justice on conditions to be determined in each case by the General Assembly upon the recommendation of the Security Council."))
                (article 94 (olq "Each Member of the United Nations undertakes to comply with the decision of the International Court of Justice in any case to which it is a party."
                                 "If any party to a case fails to perform the obligations incumbent upon it under a judgment rendered by the Court, the other party may have recourse to the Security Council, which may, if it deems necessary, make recommendations or decide upon measures to be taken to give effect to the judgment."))
                (article 95 "Nothing in the present Charter shall prevent Members of the United Nations from entrusting the solution of their differences to other tribunals by virtue of agreements already in existence or which may be concluded in the future.")
                (article 96 (olq "The General Assembly or the Security Council may request the International Court of Justice to give an advisory opinion on any legal question."
                                 "Other organs of the United Nations and specialized agencies, which may at any time be so authorized by the General Assembly, may also request advisory opinions of the Court on legal questions arising within the scope of their activities.")))
       (chapter 15 "The secretariat"
                (article 97 "The Secretariat shall comprise a Secretary General and such staff as the Organization may require. The Secretary-General shall be appointed by the General Assembly upon the recommendation of the Security Council. He shall be the chief administrative officer of the Organization.")
                (article 98 "The Secretary-General shall act in that capacity in all meetings of the General Assembly, of the Security Council, of the Economic and Social Council, and of the Trusteeship Council, and shall perform such other functions as are entrusted to him by these organs. The Secretary-General shall make an annual report to the General Assembly on the work of the Organization.")
                (article 99 "The Secretary-General may bring to the attention of the Security Council any matter which in his opinion may threaten the maintenance of international peace and security.")
                (article 100 (olq "In the performance of their duties the Secretary-General and the staff shall not seek or receive instructions from any government or from any other authority external to the Organization. They shall refrain from any action which might reflect on their position as international officials responsible only to the Organization."
                                  "Each Member of the United Nations undertakes to respect the exclusively international character of the responsibilities of the Secretary General and the staff and not to seek to influence them in the discharge of their responsibilities."))
                (article 101 (olq "The staff shall be appointed by the Secretary-General under regulations established by the General Assembly."
                                  "Appropriate staffs shall be permanently assigned to the Economic and Social Council, the Trusteeship Council, and, as required, to other organs of the United Nations. These staffs shall form a part of the Secretariat."
                                  "The paramount consideration in the employment of the staff and in the determination of the conditions of service shall be the necessity of securing the highest standards of efficiency, competence, and integrity. Due regard shall be paid to the importance of recruiting the staff on as wide a geographical basis as possible.")))
       (chapter 16 "Miscellaneous provisions"
                (article 102 (olq "Every treaty and every international agreement entered into by any Member of the United Nations after the present Charter comes into force shall as soon as possible be registered with the Secretariat and published by it."
                                  "No party to any such treaty or international agreement which has not been registered in accordance with the provisions of paragraph 1 of this Article may invoke that treaty or agreement before any organ of the United Nations."))
                (article 103 "In the event of a conflict between the obligations of the Members of the United Nations under the present Charter and their obligations under any other international agreement, their obligations under the present Charter shall prevail.")
                (article 104 "The Organization shall enjoy in the territory of each of its Members such legal capacity as may be necessary for the exercise of its functions and the fulfillment of its purposes.")
                (article 105 (olq "The Organization shall enjoy in the territory of each of its Members such privileges and immunities as are necessary for the fulfillment of its purposes."
                                  "Representatives of the Members of the United Nations and officials of the Organization shall similarly enjoy such privileges and immunities as are necessary for the independent exercise of their functions in connection with the Organization."
                                  "The General Assembly may make recommendations with a view to determining the details of the application of paragraphs 1 and 2 of this Article or may propose conventions to the Members of the United Nations for this purpose.")))
       (chapter 17 "Transitional security arrangements"
                (article 106 "Pending the coming into force of such special agreements referred to in " (link :a 43) " as in the opinion of the Security Council enable it to begin the exercise of its responsibilities under " (link :a 42) ", the parties to the Four-Nation Declaration, signed at Moscow, October 30, 1943, and France, shall, in accordance with the provisions of paragraph 5 of that Declaration, consult with one another and as occasion requires with other Members of the United Nations with a view to such joint action on behalf of the Organization as may be necessary for the purpose of maintaining international peace and security. ")
                (article 107 "Nothing in the present Charter shall invalidate or preclude action, in relation to any state which during the Second World War has been an enemy of any signatory to the present Charter, taken or authorized as a result of that war by the Governments having responsibility for such action."))
       (chapter 18 "Amendments"
                (article 108 "Amendments to the present Charter shall come into force for all Members of the United Nations when they have been adopted by a vote of two thirds of the members of the General Assembly and ratified in accordance with their respective constitutional processes by two thirds of the Members of the United Nations, including all the permanent members of the Security Council.")
                (article 109 (olq "A General Conference of the Members of the United Nations for the purpose of reviewing the present Charter may be held at a date and place to be fixed by a two-thirds vote of the members of the General Assembly and by a vote of any seven members of the Security Council. Each Member of the United Nations shall have one vote in the conference."
                                  "Any alteration of the present Charter recommended by a two-thirds vote of the conference shall take effect when ratified in accordance with their respective constitutional processes by two thirds of the Members of the United Nations including all the permanent members of the Security Council."
                                  "If such a conference has not been held before the tenth annual session of the General Assembly following the coming into force of the present Charter, the proposal to call such a conference shall be placed on the agenda of that session of the General Assembly, and the conference shall be held if so decided by a majority vote of the members of the General Assembly and by a vote of any seven members of the Security Council.")))
       (chapter 19 "Ratification and signature"
                (article 110 (olq "The present Charter shall be ratified by the signatory states in accordance with their respective constitutional processes."
                                  "The ratifications shall be deposited with the Government of the United States of America, which shall notify all the signatory states of each deposit as well as the Secretary-General of the Organization when he has been appointed."
                                  "The present Charter shall come into force upon the deposit of ratifications by the Republic of China, France, the Union of Soviet Socialist Republics, the United Kingdom of Great Britain and Northern Ireland, and the United States of America, and by a majority of the other signatory states. A protocol of the ratifications deposited shall thereupon be drawn up by the Government of the United States of America which shall communicate copies thereof to all the signatory states."
                                  "The states signatory to the present Charter which ratify it after it has come into force will become original Members of the United Nations on the date of the deposit of their respective ratifications."))
                (article 111
                         (x-p "The present Charter, of which the Chinese, French, Russian, English, and Spanish texts are equally authentic, shall remain deposited in the archives of the Government of the United States of America. Duly certified copies thereof shall be transmitted by that Government to the Governments of the other signatory states.")
                         (x-p (x-span :style "font-variant:small-caps" "In faith whereof") " the representatives of the Governments of the United Nations have signed the present Charter.")
                         (x-p (x-span :style "font-variant:small-caps" "Done") " at the city of San Francisco the twenty-sixth day of June, one thousand nine hundred and forty-five.")))
       (x-hr)
       (x-h2 "STATUTE OF THE INTERNATIONAL COURT OF JUSTICE")
       (articlej 1 (x-span :style "font-variant:small-caps" "The International Court of Justice") " established by the Charter of the United Nations as the principal judicial organ of the United Nations shall be constituted and shall function in accordance with the provisions of the present Statute.")
       (chapterj 1 "Organization of the court"
                 (articlej 2 "The Court shall be composed of a body of independent judges, elected regardless of their nationality from among persons of high moral character, who possess the qualifications required in their respective countries for appointment to the highest judicial offices, or are jurisconsults of recognized competence in international law.")
                 (articlej 3 (olq "The Court shall consist of fifteen members, no two of whom may be nationals of the same state."
                                  "A person who for the purposes of membership in the Court could be regarded as a national of more than one state shall be deemed to be a national of the one in which he ordinarily exercises civil and political rights."))
                 (articlej 4 (olq "The members of the Court shall be elected by the General Assembly and by the Security Council from a list of persons nominated by the national groups in the Permanent Court of Arbitration, in accordance with the following provisions."
                                  "In the case of Members of the United Nations not represented in the Permanent Court of Arbitration, candidates shall be nominated by national groups appointed for this purpose by their governments under the same conditions as those prescribed for members of the Permanent Court of Arbitration by Article 44 of the Convention of The Hague of 1907 for the pacific settlement of international disputes."
                                  "The conditions under which a state which is a party to the present Statute but is not a Member of the United Nations may participate in electing the members of the Court shall, in the absence of a special agreement, be laid down by the General Assembly upon recommendation of the Security Council."))
                 (articlej 5 (olq (lq "At least three months before the date of the election, the Secretary-General of the United Nations shall address a written request to the members of the Permanent Court of Arbitration belonging to the states which are parties to the present Statute, and to the members of the national groups appointed under " (link :aj 4) ", paragraph 2, inviting them to undertake, within a given time, by national groups, the nomination of persons in a position to accept the duties of a member of the Court."
                                      "No group may nominate more than four persons, not more than two of whom shall be of their own nationality. In no case may the number of candidates nominated by a group be more than double the number of seats to be filled.")))
                 (articlej 6 "Before making these nominations, each national group is recommended to consult its highest court of justice, its legal faculties and schools of law, and its national academies and national sections of international academies devoted to the study of law.")
                 (articlej 7 (olq (lq "The Secretary-General shall prepare a list in alphabetical order of all the persons thus nominated. Save as provided in " (link :aj 12) ", paragraph 2, these shall be the only persons eligible.")
                                  "The Secretary-General shall submit this list to the General Assembly and to the Security Council."))
                 (articlej 8 "The General Assembly and the Security Council shall proceed independently of one another to elect the members of the Court.")
                 (articlej 9 "At every election, the electors shall bear in mind not only that the persons to be elected should individually possess the qualifications required, but also that in the body as a whole the representation of the main forms of civilization and of the principal legal systems of the world should be assured.")
                 (articlej 10 (olq "Those candidates who obtain an absolute majority of votes in the General Assembly and in the Security Council shall be considered as elected."
                                   (lq "Any vote of the Security Council, whether for the election of judges or for the appointment of members of the conference envisaged in " (link :aj 12) ", shall be taken without any distinction between permanent and non-permanent members of the Security Council.")
                                   "In the event of more than one national of the same state obtaining an absolute majority of the votes both of the General Assembly and of the Security Council, the eldest of these only shall be considered as elected."))
                 (articlej 11 "If, after the first meeting held for the purpose of the election, one or more seats remain to be filled, a second and, if necessary, a third meeting shall take place.")
                 (articlej 12 (olq "If, after the third meeting, one or more seats still remain unfilled, a joint conference consisting of six members, three appointed by the General Assembly and three by the Security Council, may be formed at any time at the request of either the General Assembly or the Security Council, for the purpose of choosing by the vote of an absolute majority one name for each seat still vacant, to submit to the General Assembly and the Security Council for their respective acceptance."
                                   (lq "If the joint conference is unanimously agreed upon any person who fulfils the required conditions, he maybe included in its list, even though he was not included in the list of nominations referred to in " (link :aj 7) ".")
                                   "If the joint conference is satisfied that it will not be successful in procuring an election, those members of the Court who have already been elected shall, within a period to be fixed by the Security Council, proceed to fill the vacant seats by selection from among those candidates who have obtained votes either in the General Assembly or in the Security Council."
                                   "In the event of an equality of votes among the judges, the eldest judge shall have a casting vote."))
                 (articlej 13 (olq "The members of the Court shall be elected for nine years and may be re-elected; provided, however, that of the judges elected at the first elec tion, the terms of five judges shall expire at the end of three years and the terms of five more judges shall expire at the end of six years."
                                   "The judges whose terms are to expire at the end of the above-mentioned initial periods of three and six years shall be chosen by lot to be drawn by the Secretary-General immediately after the first election has been completed."
                                   "The members of the Court shall continue to discharge their duties until their places have been filled. Though replaced, they shall finish any cases which they may have begun."
                                   "In the case of the resignation of a member of the Court, the resignation shall be addressed to the President of the Court for transmission to the Secretary-General. This last notification makes the place vacant."))
                 (articlej 14 "Vacancies shall be filled by the same method as that laid down for the first election, subject to the following provision: the Secretary-General shall, within one month of the occurrence of the vacancy, proceed to issue the invitations provided for in " (link :aj 5) ", and the date of the election shall be fixed by the Security Council.")
                 (articlej 15 "A member of the Court elected to replace a member whose term of office has not expired shall hold office for the remainder of his predecessor's term.")
                 (articlej 16 (olq "No member of the Court may exercise any political or administrative function, or engage in any other occupation of a professional nature."
                                   "Any doubt on this point shall be settled by the decision of the Court."))
                 (articlej 17 (olq "No member of the Court may act as agent, counsel, or advocate in any case."
                                   "No member may participate in the decision of any case in which he has previously taken part as agent, counsel, or advocate for one of the parties, or as a member of a national or international court, or of a commission of enquiry, or in any other capacity."
                                   "Any doubt on this point shall be settled by the decision of the Court."))
                 (articlej 18 (olq "No member of the Court can be dismissed unless, in the unanimous opinion of the other members, he has ceased to fulfil the required conditions."
                                   "Formal notification thereof shall be made to the Secretary-General by the Registrar."
                                   "This notification makes the place vacant."))
                 (articlej 19 "The members of the Court, when engaged on the business of the Court, shall enjoy diplomatic privileges and immunities.")
                 (articlej 20 "Every member of the Court shall, before taking up his duties, make a solemn declaration in open court that he will exercise his powers impartially and conscientiously.")
                 (articlej 21 (olq "The Court shall elect its President and Vice-President for three years; they may be re-elected."
                                   "The Court shall appoint its Registrar and may provide for the appointment of such other officers as may be necessary."))
                 (articlej 22 (olq "The seat of the Court shall be established at The Hague. This, however, shall not prevent the Court from sitting and exercising its functions elsewhere whenever the Court considers it desirable."
                                   "The President and the Registrar shall reside at the seat of the Court."))
                 (articlej 23 (olq "The Court shall remain permanently in session, except during the judicial vacations, the dates and duration of which shall be fixed by the Court."
                                   "Members of the Court are entitled to periodic leave, the dates and duration of which shall be fixed by the Court, having in mind the distance between The Hague and the home of each judge."
                                   "Members of the Court shall be bound, unless they are on leave or prevented from attending by illness or other serious reasons duly explained to the President, to hold themselves permanently at the disposal of the Court."))
                 (articlej 24 (olq "If, for some special reason, a member of the Court considers that he should not take part in the decision of a particular case, he shall so inform the President."
                                   "If the President considers that for some special reason one of the members of the Court should not sit in a particular case, he shall give him notice accordingly."
                                   "If in any such case the member of the Court and the President disagree, the matter shall be settled by the decision of the Court."))
                 (articlej 25 (olq "The full Court shall sit except when it is expressly provided otherwise in the present Statute."
                                   "Subject to the condition that the number of judges available to constitute the Court is not thereby reduced below eleven, the Rules of the Court may provide for allowing one or more judges, according to circumstances and in rotation, to be dispensed from sitting."
                                   "A quorum of nine judges shall suffice to constitute the Court."))
                 (articlej 26 (olq "The Court may from time to time form one or more chambers, composed of three or more judges as the Court may determine, for dealing with particular categories of cases; for example, labor cases and cases relating to transit and communications."
                                   "The Court may at any time form a chamber for dealing with a particular case. The number of judges to constitute such a chamber shall be determined by the Court with the approval of the parties."
                                   "Cases shall be heard and determined by the chambers provided for in this Article if the parties so request."))
                 (articlej 27 "A judgment given by any of the chambers provided for in Articles " (link :aj 26 T) " and " (link :aj 29 T) " shall be considered as rendered by the Court.")
                 (articlej 28 "The chambers provided for in Articles " (link :aj 26 T) " and " (link :aj 29 T) " may, with the consent of the parties, sit and exercise their functions elsewhere than at The Hague.")
                 (articlej 29 "With a view to the speedy despatch of business, the Court shall form annually a chamber composed of five judges which, at the request of the parties, may hear and determine cases by summary procedure. In addition, two judges shall be selected for the purpose of replacing judges who find it impossible to sit.")
                 (articlej 30 (olq "The Court shall frame rules for carrying out its functions. In particular, it shall lay down rules of procedure."
                                   "The Rules of the Court may provide for assessors to sit with the Court or with any of its chambers, without the right to vote."))
                 (articlej 31 (olq "Judges of the nationality of each of the parties shall retain their right to sit in the case before the Court."
                                   (lq "If the Court includes upon the Bench a judge of the nationality of one of the parties, any other party may choose a person to sit as judge. Such person shall be chosen preferably from among those persons who have been nominated as candidates as provided in Articles " (link :aj 4 T) " and " (link :aj 5 T) ".")
                                   "If the Court includes upon the Bench no judge of the nationality of the parties, each of these parties may proceed to choose a judge as provided in paragraph 2 of this Article."
                                   (lq "The provisions of this Article shall apply to the case of Articles " (link :aj 26 T) " and " (link :aj 29 T) ". In such cases, the President shall request one or, if necessary, two of the members of the Court forming the chamber to give place to the members of the Court of the nationality of the parties concerned, and, failing such, or if they are unable to be present, to the judges specially chosen by the parties.")
                                   "Should there be several parties in the same interest, they shall, for the purpose of the preceding provisions, be reckoned as one party only. Any doubt upon this point shall be settled by the decision of the Court."
                                   (lq "Judges chosen as laid down in paragraphs 2,3, and 4 of this Article shall fulfil the conditions required by Articles " (link :aj 2 T) ", " (link :aj 17 T) " (paragraph 2), " (link :aj 20 T) ", and " (link :aj 24 T) " of the present Statute. They shall take part in the decision on terms of complete equality with their colleagues.")))
                 (articlej 32 (olq "Each member of the Court shall receive an annual salary."
                                   "The President shall receive a special annual allowance."
                                   "The Vice-President shall receive a special allowance for every day on which he acts as President."
                                   (lq "The judges chosen under " (link :aj 31) ", other than members of the Court, shall receive compensation for each day on which they exercise their functions.")
                                   "These salaries, allowances, and compensation shall be fixed by the General Assembly. They may not be decreased during the term of office."
                                   "The salary of the Registrar shall be fixed by the General Assembly on the proposal of the Court."
                                   "Regulations made by the General Assembly shall fix the conditions under which retirement pensions may be given to members of the Court and to the Registrar, and the conditions under which members of the Court and the Registrar shall have their traveling expenses refunded."
                                   "The above salaries, allowances, and compensation shall be free of all taxation."))
                 (articlej 33 "The expenses of the Court shall be borne by the United Nations in such a manner as shall be decided by the General Assembly."))
       (chapterj 2 "Competence of the Court"
                 (articlej 34 (olq "Only states may be parties in cases before the Court."
                                   "The Court, subject to and in conformity with its Rules, may request of public international organizations information relevant to cases before it, and shall receive such information presented by such organizations on their own initiative."
                                   "Whenever the construction of the constituent instrument of a public international organization or of an international convention adopted thereunder is in question in a case before the Court, the Registrar shall so notify the public international organization concerned and shall communicate to it copies of all the written proceedings."))
                 (articlej 35 (olq "The Court shall be open to the states parties to the present Statute."
                                   "The conditions under which the Court shall be open to other states shall, subject to the special provisions contained in treaties in force, be laid down by the Security Council, but in no case shall such conditions place the parties in a position of inequality before the Court."
                                   "When a state which is not a Member of the United Nations is a party to a case, the Court shall fix the amount which that party is to contribute towards the expenses of the Court. This provision shall not apply if such state is bearing a share of the expenses of the Court."))
                 (articlej 36 (olq "The jurisdiction of the Court comprises all cases which the parties refer to it and all matters specially provided for in the Charter of the United Nations or in treaties and conventions in force."
                                   (lq "The states parties to the present Statute may at any time declare that they recognize as compulsory <i>ipso facto</i> and without special agreement, in relation to any other state accepting the same obligation, the jurisdiction of the Court in all legal disputes concerning:"
                                       (olabcq "the interpretation of a treaty;"
                                               "any question of international law;"
                                               "the existence of any fact which, if established, would constitute a breach of an international obligation;"
                                               "the nature or extent of the reparation to be made for the breach of an international obligation."))
                                   "The declarations referred to above may be made unconditionally or on condition of reciprocity on the part of several or certain states, or for a certain time."
                                   "Such declarations shall be deposited with the Secretary-General of the United Nations, who shall transmit copies thereof to the parties to the Statute and to the Registrar of the Court."
                                   (lq "Declarations made under " (link :aj 36) " of the Statute of the Permanent Court of International Justice and which are still in force shall be deemed, as between the parties to the present Statute, to be acceptances of the compulsory jurisdiction of the International Court of Justice for the period which they still have to run and in accordance with their terms.")
                                   "In the event of a dispute as to whether the Court has jurisdiction, the matter shall be settled by the decision of the Court."))
                 (articlej 37 "Whenever a treaty or convention in force provides for reference of a matter to a tribunal to have been instituted by the League of Nations, or to the Permanent Court of International Justice, the matter shall, as between the parties to the present Statute, be referred to the International Court of Justice.")
                 (articlej 38
                           "The Court, whose function is to decide in accordance with international law such disputes as are submitted to it, shall apply:"
                           (olabcq "international conventions, whether general or particular, establishing rules expressly recognized by the contesting states;"
                                   "international custom, as evidence of a general practice accepted as law;"
                                   "the general principles of law recognized by civilized nations;"
                                   (lq "subject to the provisions of " (link :aj 59) ", judicial decisions and the teachings of the most highly qualified publicists of the various nations, as subsidiary means for the determination of rules of law."))
                           "This provision shall not prejudice the power of the Court to decide a case <i>ex aequo et bono</i>, if the parties agree thereto."))
       (chapterj 3 "Procedure"
                 (articlej 39 (olq "The official languages of the Court shall be French and English. If the parties agree that the case shall be conducted in French, the judgment shall be delivered in French. If the parties agree that the case shall be conducted in English, the judgment shall be delivered in English."
                                   "In the absence of an agreement as to which language shall be employed, each party may, in the pleadings, use the language which it prefers; the decision of the Court shall be given in French and English. In this case the Court shall at the same time determine which of the two texts shall be considered as authoritative."
                                   "The Court shall, at the request of any party, authorize a language other than French or English to be used by that party."))
                 (articlej 40 (olq "Cases are brought before the Court, as the case may be, either by the notification of the special agreement or by a written application addressed to the Registrar. In either case the subject of the dispute and the parties shall be indicated."
                                   "The Registrar shall forthwith communicate the application to all concerned."
                                   "He shall also notify the Members of the United Nations through the Secretary-General, and also any other states entitled to appear before the Court."))
                 (articlej 41 (olq "The Court shall have the power to indicate, if it considers that circumstances so require, any provisional measures which ought to be taken to preserve the respective rights of either party."
                                   "Pending the final decision, notice of the measures suggested shall forthwith be given to the parties and to the Security Council."))
                 (articlej 42 (olq "The parties shall be represented by agents."
                                   "They may have the assistance of counsel or advocates before the Court."
                                   "The agents, counsel, and advocates of parties before the Court shall enjoy the privileges and immunities necessary to the independent exercise of their duties."))
                 (articlej 43 (olq "The procedure shall consist of two parts: written and oral."
                                   "The written proceedings shall consist of the communication to the Court and to the parties of memorials, counter-memorials and, if necessary, replies; also all papers and documents in support."
                                   "These communications shall be made through the Registrar, in the order and within the time fixed by the Court."
                                   "A certified copy of every document produced by one party shall be communicated to the other party."
                                   "The oral proceedings shall consist of the hearing by the Court of witnesses, experts, agents, counsel, and advocates."))
                 (articlej 44 (olq "For the service of all notices upon persons other than the agents, counsel, and advocates, the Court shall apply direct to the government of the state upon whose territory the notice has to be served."
                                   "The same provision shall apply whenever steps are to be taken to procure evidence on the spot."))
                 (articlej 45 "The hearing shall be under the control of the President or, if he is unable to preside, of the Vice-President; if neither is able to preside, the senior judge present shall preside.")
                 (articlej 46 "The hearing in Court shall be public, unless the Court shall decide otherwise, or unless the parties demand that the public be not admitted.")
                 (articlej 47 (olq "Minutes shall be made at each hearing and signed by the Registrar and the President."
                                   "These minutes alone shall be authentic."))
                 (articlej 48 "The Court shall make orders for the conduct of the case, shall decide the form and time in which each party must conclude its arguments, and make all arrangements connected with the taking of evidence.")
                 (articlej 49 "The Court may, even before the hearing begins, call upon the agents to produce any document or to supply any explanations. Formal note shall be taken of any refusal.")
                 (articlej 50 "The Court may, at any time, entrust any individual, body, bureau, commission, or other organization that it may select, with the task of carrying out an enquiry or giving an expert opinion.")
                 (articlej 51 "During the hearing any relevant questions are to be put to the witnesses and experts under the conditions laid down by the Court in the rules of procedure referred to in " (link :aj 30) ".")
                 (articlej 52 "After the Court has received the proofs and evidence within the time specified for the purpose, it may refuse to accept any further oral or written evidence that one party may desire to present unless the other side consents.")
                 (articlej 53 (olq "Whenever one of the parties does not appear before the Court, or fails to defend its case, the other party may call upon the Court to decide in favor of its claim."
                                   (lq "The Court must, before doing so, satisfy itself, not only that it has jurisdiction in accordance with Articles " (link :aj 36 T) " and " (link :aj 37 T) ", but also that the claim is well founded in fact and law.")))
                 (articlej 54 (olq "When, subject to the control of the Court, the agents, counsel, and advocates have completed their presentation of the case, the President shall declare the hearing closed."
                                   "The Court shall withdraw to consider the judgment."
                                   "The deliberations of the Court shall take place in private and remain secret."))
                 (articlej 55 (olq "All questions shall be decided by a majority of the judges present."
                                   "In the event of an equality of votes, the President or the judge who acts in his place shall have a casting vote."))
                 (articlej 56 (olq "The judgment shall state the reasons on which it is based."
                                   "It shall contain the names of the judges who have taken part in the decision."))
                 (articlej 57 "If the judgment does not represent in whole or in part the unanimous opinion of the judges, any judge shall be entitled to deliver a separate opinion.")
                 (articlej 58 "The judgment shall be signed by the President and by the Registrar. It shall be read in open court, due notice having been given to the agents.")
                 (articlej 59 "The decision of the Court has no binding force except between the parties and in respect of that particular case.")
                 (articlej 60 "The judgment is final and without appeal. In the event of dispute as to the meaning or scope of the judgment, the Court shall construe it upon the request of any party.")
                 (articlej 61 (olq "An application for revision of a judgment may be made only when it is based upon the discovery of some fact of such a nature as to be a decisive factor, which fact was, when the judgment was given, unknown to the Court and also to the party claiming revision, always provided that such ignorance was not due to negligence."
                                   "The proceedings for revision shall be opened by a judgment of the Court expressly recording the existence of the new fact, recognizing that it has such a character as to lay the case open to revision, and declaring the application admissible on this ground."
                                   "The Court may require previous compliance with the terms of the judgment before it admits proceedings in revision."
                                   "The application for revision must be made at latest within six months of the discovery of the new fact."
                                   "No application for revision may be made after the lapse of ten years from the date of the judgment."))
                 (articlej 62 (olq "Should a state consider that it has an interest of a legal nature which may be affected by the decision in the case, it may submit a request to the Court to be permitted to intervene."
                                   "It shall be for the Court to decide upon this request."))
                 (articlej 63 (olq "Whenever the construction of a convention to which states other than those concerned in the case are parties is in question, the Registrar shall notify all such states forthwith."
                                   "Every state so notified has the right to intervene in the proceedings; but if it uses this right, the construction given by the judgment will be equally binding upon it."))
                 (articlej 64 "Unless otherwise decided by the Court, each party shall bear its own costs."))
       (chapterj 4 "Advisory opinions"
                 (articlej 65 (olq "The Court may give an advisory opinion on any legal question at the request of whatever body may be authorized by or in accordance with the Charter of the United Nations to make such a request."
                                   "Questions upon which the advisory opinion of the Court is asked shall be laid before the Court by means of a written request containing an exact statement of the question upon which an opinion is required, and accompanied by all documents likely to throw light upon the question."))
                 (articlej 66 (olq "The Registrar shall forthwith give notice of the request for an advisory opinion to all states entitled to appear before the Court."
                                   "The Registrar shall also, by means of a special and direct communication, notify any state entitled to appear before the Court or international organization considered by the Court, or, should it not be sitting, by the President, as likely to be able to furnish information on the question, that the Court will be prepared to receive, within a time limit to be fixed by the President, written statements, or to hear, at a public sitting to be held for the purpose, oral statements relating to the question."
                                   "Should any such state entitled to appear before the Court have failed to receive the special communication referred to in paragraph 2 of this Article, such state may express a desire to submit a written statement or to be heard; and the Court will decide."
                                   "States and organizations having presented written or oral statements or both shall be permitted to comment on the statements made by other states or organizations in the form, to the extent, and within the time limits which the Court, or, should it not be sitting, the President, shall decide in each particular case. Accordingly, the Registrar shall in due time communicate any such written statements to states and organizations having submitted similar statements."))
                 (articlej 67 "The Court shall deliver its advisory opinions in open court, notice having been given to the Secretary-General and to the representatives of Members of the United Nations, of other states and of international organizations immediately concerned.")
                 (articlej 68 "In the exercise of its advisory functions the Court shall further be guided by the provisions of the present Statute which apply in contentious cases to the extent to which it recognizes them to be applicable."))
       (chapterj 5 "Amendment"
                 (articlej 69 "Amendments to the present Statute shall be effected by the same procedure as is provided by the Charter of the United Nations for amendments to that Charter, subject however to any provisions which the General Assembly upon recommendation of the Security Council may adopt concerning the participation of states which are parties to the present Statute but are not Members of the United Nations.")
                 (articlej 70 "The Court shall have power to propose such amendments to the present Statute as it may deem necessary, through written communications to the Secretary-General, for consideration in conformity with the provisions of " (link :aj 69) ".")))))))

