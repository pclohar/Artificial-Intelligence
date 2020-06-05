; Start With - (reset)
;              (run)
; Food and Restaurant Expert System in CLIPS - By Pallavi Lohar (U36047678) and Sumeet Bhadale (U66934872)
; Brief Description - The program below asks a series of questions to the user and helps to suggest a food (either meal/ dessert/ drinks) 
; with the name of the restaurant we can find this food at.

; Below is a sample output of the program

;---------------------------------------------------------------------------------------------
;                 WELCOME TO THE TAMPA FOOD AND RESTAURANT RECOMMENDER SYSTEM                 
;---------------------------------------------------------------------------------------------

;Please enter your name : Monica 

;Hello, Monica Let me help you with suggesting your meal and restaurant today.

;What kind of food do you wish to have at this hour? (meal / dessert/ drinks): meal

;How much do you want to spend? ($/ $$): $$

;What kind of cuisine would you prefer (indian/ american/ italian/ thai): indian

;What should be the main base of the meal? (bread_based/ gravy_based/ rice_based): gravy_based

;What kind of meal flavour will you prefer? (tangy/ creamy/ masala): masala

;Would you prefer meat? (meat/ no_meat): no_meat

;Hi, Monica. You can enjoy Paneer Tikka Masala at Minerva Indian Restaurant (Rating : 4.1)

;---------------------------------------------------------------------------------------------
; MEAL CLASS WHICH CONSISTS ALL THE ATTRIBUTES OF A MEAL
(defclass MEAL
  (is-a USER)
  (role concrete)
  (slot cooking_type)
  (slot meal_flavour)
  (slot meal_base)
  (slot meat_preference) 
 
)

; DESSERTS CLASS CONSISTING OF THE TYPE OF DESSERT AND THE FLAVOUR
(defclass DESSERT
  (is-a USER)
  (role concrete)
  (slot dessert_type) 
  (slot dessert_flavour)
)

; DRINKS CLASS CONSISTING OF THE TYPE OF DRINK AND THE DRINK FLAVOUR
(defclass DRINKS
  (is-a USER)
  (role concrete)
  (slot beverage_type)
  (slot beverage_flavor) 
)

; RESTAURANT CLASS CONSISTS OF THE NAME OF THE RESTAURANT AND RATING ALONG THE PARTICULAR FOOD WHICH WOULD
; BE RECOMMENDED TO THE USER ON THE BASIS OF A SET OF QUESTIONS RELATED TO HIS PREFERRED MEAL,
; DESSERT OR DRINK CHOICES.
(defclass RESTAURANT
  (is-a USER)
  (role concrete)
  (slot recommended_restaurant)
  (slot rating)
  (slot meal)
  (slot dessert)     
  (slot drink) 
)

; PERSON CLASS CONSISTS OF THE USER NAME AND HIS PREFERENCES RELATED TO THE FOODTYPE,BUDGET AND CUISINE.
(defclass PERSON 
  (is-a USER)
  (role concrete)
  (slot price_cap)
  (slot food_type)
  (slot cuisine) 
  (slot user_name)
)

; DEFINING INSTANCES OF THE PERSON CLASS
(definstances PERSON-INSTANCES
  (user of PERSON)
)

; DEFINING INSTANCES OF THE RESTAURANT CLASS
(definstances RESTAURANT-INSTANCES
  (restaurant of RESTAURANT)  
)

; DEFINING INSTANCES OF THE MEAL CLASS
(definstances MEAL-INSTANCES
  (meal of MEAL)
)

; DEFINING INSTANCES OF THE DESSERT CLASS
(definstances DESSERT-INSTANCES
  (dessert of DESSERT)
)

; DEFINING INSTANCES OF THE DRINKS CLASS
(definstances DRINKS-INSTANCES
  (drinks of DRINKS)
)


; ASK QUESTION ROUTINE TO ASK QUESTIONS TO THE USER RELATED TO THE FOOD AND RESTAURANT PREFERENCES
(deffunction ask-question (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

; FUNCTION TO ASK USER HIS/HER NAME AND STORE IT.
(deffunction user-input-name(?name)
   (printout t ?name)
   (bind ?answer (read))
)
     
; RULE TO GET THE USER INPUT
(defrule get_user_name (declare (salience 10))
    =>
    (printout t crlf)
    (printout t "---------------------------------------------------------------------------------------------" crlf)
    (printout t "                 WELCOME TO THE TAMPA FOOD AND RESTAURANT RECOMMENDER SYSTEM                 " crlf)
    (printout t "---------------------------------------------------------------------------------------------" crlf)
    (printout t crlf)   
    (send [user] put-user_name
    (user-input-name "Please enter your name : ")))

; RULE TO GET THE USER'S PREFERRED FOOD TYPE
(defrule get_food_type (declare (salience 9))
  (object (is-a PERSON) (user_name ?usr ))
  =>
    (printout t crlf)  
    (printout t "Hello, " ?usr "! Let me help you with suggesting your meal and restaurant today." crlf)  
    (printout t crlf)  
    (send [user] put-food_type
    (ask-question "What kind of food do you wish to have at this hour? (meal / dessert/ drinks): " meal dessert drinks)))

; RULE TO GET THE USER'S BUDGET
(defrule get_price_cap (declare (salience 8))
(object (is-a PERSON) (food_type meal))
  => 
  (printout t crlf)
  (send [user] put-price_cap
  (ask-question "How much do you want to spend? ($/ $$): " $ $$)))

; RULE TO GET THE USER'S PREFERRED CUISINE
(defrule get_cuisine(declare (salience 7))
(object (is-a PERSON) (food_type meal))
  => 
  (printout t crlf)
  (send [user] put-cuisine
  (ask-question "What kind of cuisine would you prefer (indian/ american/ italian/ thai): " indian american italian thai)))

; RULE TO GET THE USER'S PREFERRED MEAL BASE IF CHOSEN CUISINE IS INDIAN
(defrule get_meal_base_indian
  (object (is-a PERSON) (food_type meal) (cuisine indian))
  => 
  (printout t crlf)
  (send [meal] put-meal_base
  (ask-question "What should be the main base of the meal? (bread_based/ gravy_based/ rice_based): " 
    bread_based gravy_based rice_based)))

; RULE TO GET THE USER'S PREFERRED MEAL BASE IF CHOSEN CUISINE IS AMERICAN
(defrule get_meal_base_american
  (object (is-a PERSON) (food_type meal) (cuisine american))
  => 
  (printout t crlf)
  (send [meal] put-meal_base
  (ask-question "What should be the main base of the meal? (bread_based/ no_base): " 
    bread_based no_base)))

; RULE TO GET THE USER'S PREFERRED MEAL BASE IF CHOSEN CUISINE IS ITALIAN
(defrule get_meal_base_italian
  (object (is-a PERSON) (food_type meal) (cuisine italian))
  => 
  (printout t crlf)
  (send [meal] put-meal_base
  (ask-question "What should be the main base of the meal? (bread_based/ pasta_based): " 
    bread_based pasta_based)))

; RULE TO GET THE USER'S PREFERRED MEAL BASE IF CHOSEN CUISINE IS THAI
(defrule get_meal_base_thai
  (object (is-a PERSON) (food_type meal) (cuisine thai))
  => 
  (printout t crlf)
  (send [meal] put-meal_base
  (ask-question "What should be the main base of the meal? (noodle_based / curry_based): " 
    noodle_based curry_based)))


; RULE TO GET THE USER'S MEAT PREFERENCE
(defrule get_meat_preference
 (object (is-a PERSON) (food_type meal))
  => 
  (printout t crlf)
  (send [meal] put-meat_preference
  (ask-question "Would you prefer meat? (meat/ no_meat): " 
  meat no_meat)))

; RULE TO GET THE USER'S MEAL FLAVOUR IF CHOSEN CUISINE IS INDIAN
(defrule get_meal_flavour_indian
 (object (is-a PERSON) (food_type meal)(cuisine indian))
  => 
  (printout t crlf)
  (send [meal] put-meal_flavour
  (ask-question "What kind of meal flavour will you prefer? (tangy/ creamy/ masala): " 
  tangy creamy masala)))

; RULE TO GET THE USER'S PREFERRED COOKING TYPE IF CHOSEN CUISINE IS ITALIAN
(defrule get_cooking_type_italian
  (object (is-a PERSON) (food_type meal)(cuisine italian))
  => 
  (printout t crlf)
  (send [meal] put-cooking_type
  (ask-question "How do you want your meal? (fried / steamed_and_sauted): " 
    fried steamed_and_sauted)))

; RULE TO GET THE USER'S PREFERRED COOKING TYPE IF CHOSEN CUISINE IS AMERICAN AND BREAD BASED MEAL
(defrule get_cooking_type_american_bread
  (and(object (is-a PERSON) (food_type meal)(cuisine american))
  (object (is-a MEAL) (meal_base bread_based)))
  => 
  (printout t crlf)
  (send [meal] put-cooking_type
  (ask-question "How do you want your meal? (fried/ grilled): " 
    fried grilled)))

; RULE TO GET THE USER'S PREFERRED COOKING TYPE IF CHOSEN CUISINE IS AMERICAN AND NO MEAL BASE
(defrule get_cooking_type_american_no_base
  (and(object (is-a PERSON) (food_type meal)(cuisine american))
  (object (is-a MEAL) (meal_base no_base)))
  => 
  (printout t crlf)
  (send [meal] put-cooking_type
  (ask-question "How do you want your meal? (fried/ grilled/ tossed): " 
    fried grilled tossed)))

; RULE TO GET THE USER'S PREFERRED COOKING TYPE IF CHOSEN CUISINE IS THAI
(defrule get_cooking_type_thai
  (object (is-a PERSON) (food_type meal)(cuisine thai))
  => 
  (printout t crlf)
  (send [meal] put-cooking_type
  (ask-question "How do you want your meal? (tossed/ stir_fry): " 
    tossed stir_fry)))

; RULE TO GET THE USER'S PREFERRED DESSERT TYPE
(defrule get_dessert_type
  (object (is-a PERSON) (food_type dessert))
  => 
  (printout t crlf)
  (send [dessert] put-dessert_type
  (ask-question "What type of dessert would you prefer? (pastry/ batter/ baked/ frozen): " 
    pastry batter baked frozen)))

; RULE TO GET THE USER'S PREFERRED DESSERT FLAVOUR
(defrule get_dessert_flavour
  (object (is-a PERSON) (food_type dessert))
  => 
  (printout t crlf)
  (send [dessert] put-dessert_flavour
  (ask-question "What flavour would you prefer for the desert? (fruity/ chocolaty/ caramel): " 
    fruity chocolaty caramel)))

; RULE TO GET THE USER'S PREFERRED BEVERAGE TYPE
(defrule get_beverage_type
  (object (is-a PERSON) (food_type drinks))
  => 
  (printout t crlf)
  (send [drinks] put-beverage_type
  (ask-question "What type of drink would you like? (tea/ coffee/ milkshake): " 
    tea coffee milkshake)))

; RULE TO GET THE USER'S PREFERRED BEVERAGE FLAVOUR IF BEVERAGE TYPE IS TEA
(defrule get_beverage_flavour_tea
  (and(object (is-a PERSON)(food_type drinks))
  (object (is-a DRINKS)(beverage_type tea)))
  => 
  (printout t crlf)
  (send [drinks] put-beverage_flavor
  (ask-question "What kind of tea would you like? (black_tea/ green_tea/ mint_tea): " 
    black_tea green_tea mint_tea)))

; RULE TO GET THE USER'S PREFERRED BEVERAGE FLAVOUR IF BEVERAGE TYPE IS MILKSHAKE
(defrule get_beverage_flavour_milkshake
  (and(object (is-a PERSON)(food_type drinks))
  (object (is-a DRINKS)(beverage_type milkshake)))
  => 
  (printout t crlf)
  (send [drinks] put-beverage_flavor
  (ask-question "What flavour would you prefer in your milkshake drink (banana/ strawberry/ vanilla/ chocolate): " 
    banana strawberry vanilla chocolate)))

; RULE TO GET THE USER'S PREFERRED BEVERAGE FLAVOUR IF BEVERAGE TYPE IS COFFEE
(defrule get_beverage_flavour_coffee
  (and(object (is-a PERSON)(food_type drinks))
  (object (is-a DRINKS)(beverage_type coffee)))
  => 
  (printout t crlf)
  (send [drinks] put-beverage_flavor
  (ask-question "What flavour would you prefer in your coffee? (caramel/ white_mocha/ vanilla): " 
    caramel white_mocha vanilla)))

; RECOMMENDATION RULES FOR RECOMMENDING DIFFERENT MEALS AND THE BEST RESTAURANTS FOR THEM.

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_tangy_no_meat_inexp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base bread_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Kacchi Dabeli")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_tangy_no_meat_exp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base bread_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Tomato Uttapa")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_creamy_no_meat_inexp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Pav Bhaji with Crispy Buns")
  (send [restaurant] put-recommended_restaurant "Taj Indian Cuisine")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_creamy_no_meat_exp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Dal Makhni with Roti")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_masala_no_meat_inexp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Masala Pav")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_bread_masala_no_meat_exp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Bhendi Fry with Roti")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_tangy_no_meat_inexp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Idli-Wada Sambar")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_tangy_no_meat_exp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Medu-Wada Sambar")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_creamy_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Tomato Fry")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_creamy_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Paneer Butter Masala with Wheat Bread")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_masala_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Chole Bhature")
  (send [restaurant] put-recommended_restaurant "Pastries N Chaat")
  (send [restaurant] put-rating "4.0"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_gravy_masala_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base gravy_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Paneer Tikka Masala")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_tangy_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Tamarind Rice")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED TANGY MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_tangy_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour tangy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Rice with Tangy Tomato Curry")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_creamy_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Curd Rice with Cilantro")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED CREAMY MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_creamy_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour creamy)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Rice Kheer")
  (send [restaurant] put-recommended_restaurant "Saffron Indian Cuisine")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_masala_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Masala Rice")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED MASALA MEAL WITH NO MEAT
(defrule recommend_indian_meal_rice_masala_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour masala)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Shahi Pulav")
  (send [restaurant] put-recommended_restaurant "Saffron Indian Cuisine")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_bread_tangy_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Mutton Kheema with Wheat Buns")
  (send [restaurant] put-recommended_restaurant "Taj Indian Cuisine")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_bread_tangy_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Mutton Fry with Roti")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_bread_creamy_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chiken Tikka Butter Masala with Wheat Roti")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_bread_creamy_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Guntur Karam Dosa with Chicken")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN BREAD BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_bread_masala_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base bread_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Pepper Lamb with Cheese Garlic Naan")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN BREAD BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_bread_masala_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base bread_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Mutton Tikka Masala with Garlic Naan")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_tangy_meat_inexp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Mango Chicken")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_tangy_meat_exp
  (and(object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Vindaloo")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_creamy_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Rogan Josh")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_creamy_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Methi Lamb")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN GRAVY BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_masala_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Tikka Masala")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN GRAVY BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_gravy_masala_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base gravy_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Gosht Vindaloo")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))


; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_rice_tangy_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL)(meal_base rice_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Curry Fried Rice")
  (send [restaurant] put-recommended_restaurant "Southern Spices")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED TANGY MEAL WITH MEAT
(defrule recommend_indian_meal_rice_tangy_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL)(meal_base rice_based)(meal_flavour tangy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Fish Madras with Rice")
  (send [restaurant] put-recommended_restaurant "Jai Ho Royale Indian Cuisine")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_rice_creamy_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Murg Curry Pulav")
  (send [restaurant] put-recommended_restaurant "Minerva Indian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED CREAMY MEAL WITH MEAT
(defrule recommend_indian_meal_rice_creamy_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour creamy)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Butter Chicken with Rice")
  (send [restaurant] put-recommended_restaurant "Saffron Indian Cuisine")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  INEXPENSIVE INDIAN RICE BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_rice_masala_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Vijaywada Chicken Biryani")
  (send [restaurant] put-recommended_restaurant "Pastries N Chaat")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE INDIAN RICE BASED MASALA MEAL WITH MEAT
(defrule recommend_indian_meal_rice_masala_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine indian)(price_cap $$))
  (object (is-a MEAL) (meal_base rice_based)(meal_flavour masala)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Shahi Chicken Biryani")
  (send [restaurant] put-recommended_restaurant "Saffron Indian Cuisine")
  (send [restaurant] put-rating "4.3"))



; RECOMMEND MEAL -  INEXPENSIVE AMERICAN BREAD BASED FRIED MEAL NO MEAT
(defrule recommend_american_meal_bread_fried_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL)(meal_base bread_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal " Fry Bread with sweet potato, corn, black beans and jalapeno")
  (send [restaurant] put-recommended_restaurant "Alaska Mike’s Yukon Fry Bread")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN BREAD BASED FRIED MEAL NO MEAT
(defrule recommend_american_meal_bread_fried_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL)(meal_base bread_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Fried Cheese Curds")
  (send [restaurant] put-recommended_restaurant "Culver's")
  (send [restaurant] put-rating "4.4"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN BREAD BASED GRILLED MEAL NO MEAT
(defrule recommend_american_meal_bread_grilled_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type grilled)(meat_preference no_meat)))
    =>  
  (send [restaurant] put-meal "Black Bean Burger with Potato Fries")
  (send [restaurant] put-recommended_restaurant "Burger 21")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN BREAD BASED GRILLED MEAL NO MEAT
(defrule recommend_american_meal_bread_grilled_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type grilled)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Grilled Veggie Sandwich")
  (send [restaurant] put-recommended_restaurant "EATS! American Grill")
  (send [restaurant] put-rating "4.3"))



; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASE FRIED MEAL NO MEAT
(defrule recommend_american_meal_no_base_fried_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL)(meal_base no_base)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Onion Rings with Creamy Sauce")
  (send [restaurant] put-recommended_restaurant "iHop")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASE FRIED MEAL NO MEAT
(defrule recommend_american_meal_no_base_fried_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL)(meal_base no_base)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Chickpea Empanada")
  (send [restaurant] put-recommended_restaurant "Vegan International Co. Kitchen & Market")
  (send [restaurant] put-rating "4.8"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASE GRILLED MEAL NO MEAT
(defrule recommend_american_meal_no_base_grilled_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base no_base)(cooking_type grilled)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Grilled Vegan Steak")
  (send [restaurant] put-recommended_restaurant "Taco Bus")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASE GRILLED MEAL NO MEAT
(defrule recommend_american_meal_no_base_grilled_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base no_base)(cooking_type grilled)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Grilled tomatoes, jalapeno peppers, and mushrooms with waffles")
  (send [restaurant] put-recommended_restaurant "Waffle House")
  (send [restaurant] put-rating "4.0"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASE TOSSED MEAL NO MEAT
(defrule recommend_american_meal_no_base_tossed_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base no_base)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Lemon Pasta Salad")
  (send [restaurant] put-recommended_restaurant "Sweet Tomatoes")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASE TOSSED MEAL NO MEAT
(defrule recommend_american_meal_no_base_tossed_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base no_base)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Warm Goat Cheese & Spiced Pecan Salad")
  (send [restaurant] put-recommended_restaurant "The Secret Garden Cafe")
  (send [restaurant] put-rating "4.6"))



; RECOMMEND MEAL -  INEXPENSIVE AMERICAN BREAD BASED FRIED MEAL WITH MEAT
(defrule recommend_american_meal_bread_fried_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Buttermilk Fried Chicken Breast with pickle and signature Cluck Sauce")
  (send [restaurant] put-recommended_restaurant "Ciccio Restaurant Group")
  (send [restaurant] put-rating "4.4"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN BREAD BASED FRIED MEAL WITH MEAT
(defrule recommend_american_meal_bread_fried_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Buttermilk Fried Chicken Breast with pickle and signature Cluck Sauce")
  (send [restaurant] put-recommended_restaurant "Ciccio Restaurant Group")
  (send [restaurant] put-rating "4.4"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN BREAD BASED GRILLED MEAL WITH MEAT
(defrule recommend_american_meal_bread_grilled_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL)(meal_base bread_based)(cooking_type grilled)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Cuban Toast with Eggs and Corned Beef Hash")
  (send [restaurant] put-recommended_restaurant "The Cuban Sandwich Shop")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN BREAD BASED GRILLED MEAL WITH MEAT
(defrule recommend_american_meal_bread_grilled_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL)(meal_base bread_based)(cooking_type grilled)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken & Cheese Melt")
  (send [restaurant] put-recommended_restaurant "Whiskey Cake Kitchen & Bar")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASED FRIED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_fried_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base no_base)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Buffalo Shrimp with Curly French Fries")
  (send [restaurant] put-recommended_restaurant "Hooters")
  (send [restaurant] put-rating "4.5"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASED FRIED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_fried_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base no_base)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Country Fried Steak")
  (send [restaurant] put-recommended_restaurant "Cracker Barrel Old Country Store")
  (send [restaurant] put-rating "4.4"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASE GRILLED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_grilled_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base no_base)(cooking_type grilled)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Blackened Grilled Fish")
  (send [restaurant] put-recommended_restaurant "Oakley's Grille")
  (send [restaurant] put-rating "4.5"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASE GRILLED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_grilled_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base no_base)(cooking_type grilled)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "HAPPY HERD - Sampler of 4 different meats, 2 sides, 4 biscuits or cornbread")
  (send [restaurant] put-recommended_restaurant "4 Rivers Smokehouse - South Tampa BBQ")
  (send [restaurant] put-rating "4.5"))

; RECOMMEND MEAL -  INEXPENSIVE AMERICAN NO BASE TOSSED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_tossed_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $))
  (object (is-a MEAL) (meal_base no_base)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chopped Chicken Salad with Honey Mustard Vinaigrette")
  (send [restaurant] put-recommended_restaurant "Sweet Tomatoes")
  (send [restaurant] put-rating "4.2"))

; RECOMMEND MEAL -  EXPENSIVE AMERICAN NO BASE TOSSED MEAL WITH MEAT
(defrule recommend_american_meal_no_base_tossed_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine american)(price_cap $$))
  (object (is-a MEAL) (meal_base no_base)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "2 PAC'S 2 YEAR TURKEY RAP")
  (send [restaurant] put-recommended_restaurant "Fruitwood Stand Up Market")
  (send [restaurant] put-rating "4.7"))


; RECOMMEND MEAL -  INEXPENSIVE ITALIAN BREAD BASED FRIED MEAL WITH NO MEAT
(defrule recommend_italian_meal_bread_fried_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "All Vegetarian New York Style Pizza")
  (send [restaurant] put-recommended_restaurant "New York New York Pizza")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE ITALIAN BREAD BASED FRIED MEAL WITH NO MEAT
(defrule recommend_italian_meal_bread_fried_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Fried Ravioli with Garlic Breadsticks")
  (send [restaurant] put-recommended_restaurant "GINO’S Restaurant and Bar")
  (send [restaurant] put-rating "4.5"))

; RECOMMEND MEAL -  INEXPENSIVE ITALIAN BREAD BASED STEAMED AND SAUTED MEAL WITH NO MEAT
(defrule recommend_italian_meal_bread_steamed_and_sauted_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type steamed_and_sauted)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Warm Ricotta")
  (send [restaurant] put-recommended_restaurant "Ava Tampa")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  EXPENSIVE ITALIAN BREAD BASED STEAMED AND SAUTED MEAL WITH NO MEAT
(defrule recommend_italian_meal_bread_steamed_and_sauted_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type steamed_and_sauted)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Bruschetta")
  (send [restaurant] put-recommended_restaurant "Bella's Italian Cafe")
  (send [restaurant] put-rating "4.6"))


; RECOMMEND MEAL -  INEXPENSIVE ITALIAN PASTA BASED FRIED MEAL WITH NO MEAT
(defrule recommend_italian_meal_pasta_fried_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Italian Fried Pasta")
  (send [restaurant] put-recommended_restaurant "Stefano Greek Italian Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE ITALIAN PASTA BASED FRIED MEAL WITH NO MEAT
(defrule recommend_italian_meal_pasta_fried_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type fried)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Fried Ravioli")
  (send [restaurant] put-recommended_restaurant "GINO’S Restaurant and Bar")
  (send [restaurant] put-rating "4.5"))

; RECOMMEND MEAL -  INEXPENSIVE ITALIAN PASTA BASED STEAMED AND SAUTED MEAL WITH NO MEAT
(defrule recommend_italian_meal_pasta_steamed_and_sauted_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type steamed_and_sauted)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Casarecce al Pomodoro")
  (send [restaurant] put-recommended_restaurant "Ava Tampa")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  EXPENSIVE ITALIAN PASTA BASED STEAMED AND SAUTED MEAL WITH NO MEAT
(defrule recommend_italian_meal_pasta_steamed_and_sauted_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type steamed_and_sauted)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "BASIL TOMATO Pasta")
  (send [restaurant] put-recommended_restaurant "Bella's Italian Cafe")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  INEXPENSIVE ITALIAN BREAD BASED FRIED MEAL WITH MEAT
(defrule recommend_italian_meal_bread_fried_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Buffalo Chicken Pizza")
  (send [restaurant] put-recommended_restaurant "New York New York Pizza")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  EXPENSIVE ITALIAN BREAD BASED FRIED MEAL WITH MEAT
(defrule recommend_italian_meal_bread_fried_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Peroni Shrimp")
  (send [restaurant] put-recommended_restaurant "Gio Italian Grill")
  (send [restaurant] put-rating "4.0"))


; RECOMMEND MEAL -  INEXPENSIVE ITALIAN BREAD BASED STEAMED AND SAUTED MEAL WITH MEAT
(defrule recommend_italian_meal_bread_steamed_and_sauted_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type steamed_and_sauted)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Gio Antipasta")
  (send [restaurant] put-recommended_restaurant "Gio Italian Grill")
  (send [restaurant] put-rating "4.0"))

; RECOMMEND MEAL -  EXPENSIVE ITALIAN BREAD BASED STEAMED AND SAUTED MEAL WITH MEAT
(defrule recommend_italian_meal_bread_steamed_and_sauted_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base bread_based)(cooking_type steamed_and_sauted)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Abbacchio Al Forno Alla Ciociara")
  (send [restaurant] put-recommended_restaurant "Donatello Italian Restaurant")
  (send [restaurant] put-rating "4.3"))



; RECOMMEND MEAL -  INEXPENSIVE ITALIAN PASTA BASED FRIED MEAL WITH MEAT
(defrule recommend_italian_meal_pasta_fried_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Spaghetti with Meatballs")
  (send [restaurant] put-recommended_restaurant "Carrabba's Italian Grill")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  EXPENSIVE ITALIAN PASTA BASED FRIED MEAL WITH MEAT
(defrule recommend_italian_meal_pasta_fried_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type fried)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Alfredo Pasta with Meatballs")
  (send [restaurant] put-recommended_restaurant "Timpano Italian Chophouse")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  INEXPENSIVE ITALIAN PASTA BASED STEAMED AND SAUTED MEAL WITH MEAT
(defrule recommend_italian_meal_pasta_steamed_and_saute_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type steamed_and_sauted)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Piccata")
  (send [restaurant] put-recommended_restaurant "Little Italy's Family Restaurant & Catering")
  (send [restaurant] put-rating "4.3"))


; RECOMMEND MEAL -  EXPENSIVE ITALIAN PASTA BASED STEAMED AND SAUTED MEAL WITH MEAT
(defrule recommend_italian_meal_pasta_steamed_and_saute_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine italian)(price_cap $$))
  (object (is-a MEAL) (meal_base pasta_based)(cooking_type steamed_and_sauted)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Filetto Gioacchino Rossini - Filet of beef with Madeira sauce")
  (send [restaurant] put-recommended_restaurant "Carrabba's Italian Grill")
  (send [restaurant] put-rating "4.4"))


; RECOMMEND MEAL -  INEXPENSIVE THAI NOODLE BASED TOSSED MEAL WITH NO MEAT
(defrule recommend_thai_meal_noodle_tossed_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Thai Peanut Noodles")
  (send [restaurant] put-recommended_restaurant "Sawatdee Thai Cuisine")
  (send [restaurant] put-rating "4.3"))

; RECOMMEND MEAL -  EXPENSIVE THAI NOODLE BASED TOSSED MEAL WITH NO MEAT
(defrule recommend_thai_meal_noodle_tossed_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Thai Drunken Noodle")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  INEXPENSIVE THAI NOODLE BASED STIR-FRY MEAL WITH NO MEAT
(defrule recommend_thai_meal_noodle_stir_fry_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type stir_fry)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Pad See Ew - Stir Fried Soy Sauce Noodles")
  (send [restaurant] put-recommended_restaurant "Thai Sweet Basil")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE THAI NOODLE BASED STIR-FRY MEAL WITH NO MEAT
(defrule recommend_thai_meal_noodle_stir_fry_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type stir_fry)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Lad Na - Stir-fried rice noodles, broccoli and special gravy sauce")
  (send [restaurant] put-recommended_restaurant "Thai Sweet Basil")
  (send [restaurant] put-rating "3.9"))


; RECOMMEND MEAL -  INEXPENSIVE THAI CURRY BASED TOSSED WITH NO MEAT
(defrule recommend_thai_meal_curry_tossed_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Panang Curry")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  INEXPENSIVE THAI CURRY BASED TOSSED WITH NO MEAT
(defrule recommend_thai_meal_curry_tossed_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type tossed)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Stir Fry Vegetable Curry")
  (send [restaurant] put-recommended_restaurant "Asiatic Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  INEXPENSIVE THAI CURRY BASED STIR-FRY MEAL WITH NO MEAT
(defrule recommend_thai_meal_curry_stir_fry_no_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type stir_fry)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Spicy Coconut Vegetable Stir-Fry Curry")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE THAI CURRY BASED STIR-FRY MEAL WITH NO MEAT
(defrule recommend_thai_meal_curry_stir_fry_no_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type stir_fry)(meat_preference no_meat)))
    => 
  (send [restaurant] put-meal "Garlic and Black Pepper Curry Stir-Fry")
  (send [restaurant] put-recommended_restaurant "Sawatdee Thai Cuisine")
  (send [restaurant] put-rating "4.1"))


; RECOMMEND MEAL -  INEXPENSIVE THAI NOODLE BASED TOSSED WITH MEAT
(defrule recommend_thai_meal_noodle_tossed_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL)(meal_base noodle_based)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Wonton Noodles")
  (send [restaurant] put-recommended_restaurant "Asiatic Restaurant")
  (send [restaurant] put-rating "3.9"))

; RECOMMEND MEAL -  EXPENSIVE THAI NOODLE BASED TOSSED WITH MEAT
(defrule recommend_thai_meal_noodle_tossed_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL)(meal_base noodle_based)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Khao Soi Noodles")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.7"))

; RECOMMEND MEAL -  INEXPENSIVE THAI NOODLE BASED STIR-FRY MEAL WITH MEAT
(defrule recommend_thai_meal_noodle_stir_fry_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type stir_fry)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Pork Pad Ba Mee")
  (send [restaurant] put-recommended_restaurant "Sawatdee Thai Cuisine")
  (send [restaurant] put-rating "4.1"))

; RECOMMEND MEAL -  EXPENSIVE THAI NOODLE BASED STIR-FRY MEAL WITH MEAT
(defrule recommend_thai_meal_noodle_stir_fry_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base noodle_based)(cooking_type stir_fry)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Pad Thai Noodles")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.7"))

; RECOMMEND MEAL -  INEXPENSIVE THAI CURRY BASED TOSSED WITH MEAT
(defrule recommend_thai_meal_curry_tossed_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Panang Curry ")
  (send [restaurant] put-recommended_restaurant "Sawatdee Thai Cuisine")
  (send [restaurant] put-rating "4.1"))
 
; RECOMMEND MEAL -  EXPENSIVE THAI CURRY BASED TOSSED WITH MEAT
(defrule recommend_thai_meal_curry_tossed_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type tossed)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Red Curry tossed with Chicken and Shrimp")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.7"))

; RECOMMEND MEAL -  INEXPENSIVE THAI CURRY BASED STIR-FRY WITH MEAT
(defrule recommend_thai_meal_curry_stir_fry_meat_inexp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type stir_fry)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Pork Massaman Curry ")
  (send [restaurant] put-recommended_restaurant "Sawatdee Thai Cuisine")
  (send [restaurant] put-rating "4.1"))
 

; RECOMMEND MEAL -  EXPENSIVE THAI CURRY BASED STIR-FRY WITH MEAT
(defrule recommend_thai_meal_curry_stir_fry_meat_exp
  (and (object (is-a PERSON) (food_type meal)(cuisine thai)(price_cap $$))
  (object (is-a MEAL) (meal_base curry_based)(cooking_type stir_fry)(meat_preference meat)))
    => 
  (send [restaurant] put-meal "Chicken Yellow Curry")
  (send [restaurant] put-recommended_restaurant "Palm Thai Restaurant")
  (send [restaurant] put-rating "4.7"))

; Recommend Desserts - FRUITY PASTRY
(defrule recommend_dessert_pastry_fruity
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type pastry)(dessert_flavour fruity)))
  => 
  (send [restaurant] put-dessert "Strawberry Puff, Fresh Fruit Tart or Lemon bar")
  (send [restaurant] put-recommended_restaurant "Dough")
  (send [restaurant] put-rating "4.1"))


; Recommend Desserts - CHOCOLATE PASTRY
(defrule recommend_dessert_pastry_chocolaty
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type pastry)(dessert_flavour chocolaty)))
  => 
  (send [restaurant] put-dessert "Brownie, Chocolate Croissant or Chocolate Eclair")
  (send [restaurant] put-recommended_restaurant "The Dessert Spot at Toffee To Go")
  (send [restaurant] put-rating "4.8"))

; Recommend Desserts - CARAMEL PASTRY
(defrule recommend_dessert_pastry_caramel
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type pastry)(dessert_flavour caramel)))
  => 
  (send [restaurant] put-dessert "Salted Caramel Puffs")
  (send [restaurant] put-recommended_restaurant "Dough Nation")
  (send [restaurant] put-rating "4.5"))


; Recommend Desserts - FRUITY BATTER
(defrule recommend_dessert_batter_fruity
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type batter)(dessert_flavour fruity)))
  => 
  (send [restaurant] put-dessert "Blackberry Pancakes with butter syrup")
  (send [restaurant] put-recommended_restaurant "iHop")
  (send [restaurant] put-rating "4.2"))

; Recommend Desserts - CHOCOLATE BATTER
(defrule recommend_dessert_batter_chocolaty
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type batter)(dessert_flavour chocolaty)))
  => 
  (send [restaurant] put-dessert "Chocolate Crepes with whipped cream")
  (send [restaurant] put-recommended_restaurant "Denny's")
  (send [restaurant] put-rating "3.6"))

; Recommend Desserts - CARAMEL BATTER

(defrule recommend_dessert_batter_caramel
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type batter)(dessert_flavour caramel)))
  => 
  (send [restaurant] put-dessert "Salted Caramel Cookie Dough Pancakes")
  (send [restaurant] put-recommended_restaurant "Shellys Cafe")
  (send [restaurant] put-rating "4.7"))


; Recommend Desserts -  BAKED (FRUITY)
(defrule recommend_dessert_baked_fruity
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type baked)(dessert_flavour fruity)))
  => 
  (send [restaurant] put-dessert "Apple Fruit Pie")
  (send [restaurant] put-recommended_restaurant "Alessi Bakery")
  (send [restaurant] put-rating "4.1"))

; Recommend Desserts -  BAKED (CHOCOLATE)
(defrule recommend_dessert_baked_chocolaty
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type baked)(dessert_flavour chocolaty)))
  => 
  (send [restaurant] put-dessert "Chocolate macaroons or Baked Chocolate Pudding")
  (send [restaurant] put-recommended_restaurant "Le Macaron French Pastries")
  (send [restaurant] put-rating "4.2"))


; Recommend Desserts -  BAKED (CARAMEL)
(defrule recommend_dessert_baked_caramel
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type baked)(dessert_flavour caramel)))
  => 
  (send [restaurant] put-dessert "Salted Caramel Cheesecake")
  (send [restaurant] put-recommended_restaurant "Cheesecake Factory")
  (send [restaurant] put-rating "4.9"))


; Recommend Desserts - FROZEN (FRUITY)
(defrule recommend_dessert_frozen_fruity
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type frozen)(dessert_flavour fruity)))
  => 
  (send [restaurant] put-dessert "Frozen Yogurt Fruit Bites or  ")
  (send [restaurant] put-recommended_restaurant "Haagen-Dazs")
  (send [restaurant] put-rating "4.4"))

; Recommend Desserts - FROZEN (CHOCOLATE)
(defrule recommend_dessert_frozen_chocolaty
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type frozen)(dessert_flavour chocolaty)))
  => 
  (send [restaurant] put-dessert "Chocolate Devotion")
  (send [restaurant] put-recommended_restaurant "Cold Stone Creamery")
  (send [restaurant] put-rating "5"))

; Recommend Desserts - FROZEN (CARAMEL)
(defrule recommend_dessert_frozen_caramel
  (and (object (is-a PERSON) (food_type dessert))
  (object (is-a DESSERT) (dessert_type frozen)(dessert_flavour caramel)))
  => 
  (send [restaurant] put-dessert "Caramel Cookie Dough Ice Cream")
  (send [restaurant] put-recommended_restaurant "Baskin-Robbins")
  (send [restaurant] put-rating "4.0"))



; Recommend Drinks - TEA (BLACK TEA)
(defrule recommend_drink_tea_black
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type tea)(beverage_flavor black_tea)))
    => 
  (send [restaurant] put-drink "Black Peach Tea or Earl Grey")
  (send [restaurant] put-recommended_restaurant "The Empress Tea Room")
  (send [restaurant] put-rating "4.4"))

; Recommend Drinks - TEA (GREEN TEA)
(defrule recommend_drink_tea_green
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type tea)(beverage_flavor green_tea)))
    => 
  (send [restaurant] put-drink "Emperor's Cloud Mist or Matcha Green Tea")
  (send [restaurant] put-recommended_restaurant "Starbucks")
  (send [restaurant] put-rating "4.5"))

; Recommend Drinks - TEA (MINT TEA)
(defrule recommend_drink_tea_mint
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type tea)(beverage_flavor mint_tea)))
    => 
  (send [restaurant] put-drink "Jade Citrus Mint")
  (send [restaurant] put-recommended_restaurant "Kaleisia Tea Lounge")
  (send [restaurant] put-rating "4.7"))


; Recommend Drinks - COFFEE (CARAMEL)
(defrule recommend_drink_coffee_caramel
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type coffee)(beverage_flavor caramel)))
    => 
  (send [restaurant] put-drink "Iced Caramel Macchiato with extra caramel drizzel")
  (send [restaurant] put-recommended_restaurant "Starbucks")
  (send [restaurant] put-rating "4.5"))


; Recommend Drinks - COFFEE (WHITE MOCHA)
(defrule recommend_drink_coffee_white_mocha
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type coffee)(beverage_flavor white_mocha)))
    => 
  (send [restaurant] put-drink "Toasted White Mocha with Almond Milk")
  (send [restaurant] put-recommended_restaurant "Felicitous")
  (send [restaurant] put-rating "4.5"))

; Recommend Drinks - COFFEE (VANILLA)
(defrule recommend_drink_coffee_vanilla
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type coffee)(beverage_flavor vanilla)))
    => 
  (send [restaurant] put-drink "Vanilla Iced Coffee")
  (send [restaurant] put-recommended_restaurant "Foundation Coffee Co.")
  (send [restaurant] put-rating "4.7"))


; Recommend Drinks - MILKSHAKE (BANANA)
(defrule recommend_drink_milkshake_banana
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS)(beverage_type milkshake)(beverage_flavor banana)))
    => 
  (send [restaurant] put-drink "Banana Bobacup with Coconut Milk or Banana Milkshake")
  (send [restaurant] put-recommended_restaurant "Bo's Ice Cream")
  (send [restaurant] put-rating "4.7"))

; Recommend Drinks - MILKSHAKE (STRAWBERRY)
(defrule recommend_drink_milkshake_strawberry
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type milkshake)(beverage_flavor strawberry)))
    => 
  (send [restaurant] put-drink "Very Strawberry Milkshake")
  (send [restaurant] put-recommended_restaurant "Dairy Joy")
  (send [restaurant] put-rating "4.7"))

; Recommend Drinks - MILKSHAKE (VANILLA)
(defrule recommend_drink_milkshake_vanilla
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type milkshake)(beverage_flavor vanilla)))
    => 
  (send [restaurant] put-drink "Vanilla Fantasy Milkshake with Butterscotch")
  (send [restaurant] put-recommended_restaurant "5 Guys")
  (send [restaurant] put-rating "4.2"))

; Recommend Drinks - MILKSHAKE (CHOCOLATE)
(defrule recommend_drink_milkshake_chocolate
  (and (object (is-a PERSON) (food_type drinks))
  (object (is-a DRINKS) (beverage_type milkshake)(beverage_flavor chocolate)))
    => 
  (send [restaurant] put-drink "Pure Chocolate Milkshake or Oreo Milkshake")
  (send [restaurant] put-recommended_restaurant "Chick-Fil-A")
  (send [restaurant] put-rating "4.0"))


; RULE TO RECOMMEND A MEAL AND THE RESPECTIVE RESTAURANT FOR THAT MEAL
(defrule recommended_restaurant_meal (declare (salience -1))
  (and(object (is-a PERSON) (food_type meal)(user_name ?usr))
  (object (is-a RESTAURANT) (meal ?m)(rating ?rat)(recommended_restaurant ?res)))
  =>
  (printout t crlf)
  (printout t "Hi, " ?usr ". You can enjoy " ?m " at " ?res" (Rating : " ?rat ")" crlf))


; RULE TO RECOMMEND A DESSERT AND THE RESPECTIVE RESTAURANT FOR THAT DESSERT
(defrule recommended_restaurant_dessert (declare (salience -2))
  (and(object (is-a PERSON) (food_type dessert)(user_name ?usr))
  (object (is-a RESTAURANT) (dessert ?des)(rating ?rat)(recommended_restaurant ?res)))
  =>
  (printout t crlf)
  (printout t "Hi, " ?usr ". You can enjoy " ?des " at " ?res" (Rating : " ?rat ")" crlf))


; RULE TO RECOMMEND A DRINK AND THE RESPECTIVE RESTAURANT FOR THAT DRINK
(defrule recommended_restaurant_drinks (declare (salience -3))
  (and(object (is-a PERSON) (food_type drinks)(user_name ?usr))
  (object (is-a RESTAURANT) (drink ?d)(rating ?rat)(recommended_restaurant ?res)))
  =>
  (printout t crlf)
  (printout t "Hi, " ?usr ". You can enjoy " ?d " at " ?res" (Rating : " ?rat ")" crlf))
