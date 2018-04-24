//the first line does what's called "initialization" of a javascript object
//the object you're intializing is an array (notated by the "[]" and you're calling it data_from_shiny

var data_from_shiny = []; 

//next, you're initializing an object called "chart", to which you're assigning a billboard chart.
//bb.generate is called a "constructor"--meaning it constructs a new object.
//when you call a constructor, you need to provide all its attributes as parameters.
//that is what you're providing when you assign values to "columns","type","types","bindto",etc.
//you can put whatever you want as the values to those attributes, provided they meet the data type requirements of each attribute.
//for example, the "columns" attribute requires an array data type. the trick is to create a javascript array in shiny/R from your data frame and send it to this javascript code to read.

//Key point to understand--if you could call a read_csv function in javascript and manipulate your data frame within javascript to get the array you needed to populate the chart, there would be no reason to use R/Shiny at all (outside of hosting your javascript app on the web). The main utility we're getting out of using R/Shiny is ease of data manipulation. It is possible to read and manipulate a csv file in javascript. It's just not as clean an user-friendly as R. We're leveraging shiny/R to do that work for us.


var chart = bb.generate({
  data: {
   
    columns: data_from_shiny, //this is the data you got from shiny
    type: "bar",
    types: {
      metric: "bar"
    },
  labels: true
  
  },
    

      axis: {
    x: {
      type: "category",
      tick: {
      multiline: false
      },
        categories: ["Before Match","After Match"]
    },
     
  },
  bindto: "#CombinationChart",
  tooltip: {grouped: false},
  title: {
   text: "Covariate Comparision: Before Propensity Match vs. After Propensity Match"
  }
});



document.getElementById("CombinationChart").align = "center";

chart.data.colors({
ABD_MONTHS_PER_1000_TREATED: "#00AC05", 
ABD_MONTHS_PER_1000_UNTREATED: "#ff0000", 
AVG_ZIP_PERCENT_URBAN_TREATED: "#00AC05", AVG_ZIP_PERCENT_URBAN_UNTREATED: "#ff0000", AVG_ZIP_PERCENT_WHITE_TREATED: "#00AC05", AVG_ZIP_PERCENT_WHITE_UNTREATED: "#ff0000", PREV_ATTEMPTS_PER_1000_TREATED: "#00AC05", PREV_ATTEMPTS_PER_1000_UNTREATED: "#ff0000", P_ABD_FLAG_TREATED: "#00AC05", P_ABD_FLAG_UNTREATED: "#ff0000", P_ABUSE_NEGLECT_TREATED: "#00AC05", P_ABUSE_NEGLECT_UNTREATED: "#ff0000", P_AVOIDABLE_ED_LAST_12_FLAG_TREATED: "#00AC05", P_AVOIDABLE_ED_LAST_12_FLAG_UNTREATED: "#ff0000", P_ED_VISIT_FLAG_5_TREATED: "#00AC05", P_ED_VISIT_FLAG_5_UNTREATED: "#ff0000", P_FRANKLIN_COUNTY_TREATED: "#00AC05", P_FRANKLIN_COUNTY_UNTREATED: "#ff0000", P_IP_LAST_12_FLAG_TREATED: "#00AC05", P_IP_LAST_12_FLAG_UNTREATED: "#ff0000", P_NCH_ATTRIBUTED_ASSIGNED_TREATED: "#00AC05", P_NCH_ATTRIBUTED_ASSIGNED_UNTREATED: "#ff0000", P_NCH_ED_TREATED: "#00AC05", P_NCH_ED_UNTREATED: "#ff0000", P_NCH_Inpatient_TREATED: "#00AC05", P_NCH_Inpatient_UNTREATED: "#ff0000", P_NCH_Previous_TREATED: "#00AC05", P_NCH_Previous_UNTREATED: "#ff0000", P_NOT_WITH_PARENTS_TREATED: "#00AC05", P_NOT_WITH_PARENTS_UNTREATED: "#ff0000", P_OVER_10K_PAID_TREATED: "#00AC05", P_OVER_10K_PAID_UNTREATED: "#ff0000", P_PREVIOUSLY_ENROLLED_TREATED: "#00AC05", P_PREVIOUSLY_ENROLLED_UNTREATED: "#ff0000", P_PREV_ASTHMA_ED_TREATED: "#00AC05", P_PREV_ASTHMA_ED_UNTREATED: "#ff0000", P_PREV_DIABETES_ED_TREATED: "#00AC05", P_PREV_DIABETES_ED_UNTREATED: "#ff0000", P_PRIM_DX_BH_TREATED: "#00AC05", P_PRIM_DX_BH_UNTREATED: "#ff0000", P_PRIM_DX_HCUP_CHRONIC_TREATED: "#00AC05", P_PRIM_DX_HCUP_CHRONIC_UNTREATED: "#ff0000", P_USES_PCP_TREATED: "#00AC05", P_USES_PCP_UNTREATED: "#ff0000", P_homehealth_flag_TREATED: "#00AC05", P_homehealth_flag_UNTREATED: "#ff0000"
})

Shiny.addCustomMessageHandler("get_data_from_shiny",

    function(message){
    
    // get the data you sent from shiny here
    data_from_shiny = JSON.parse(message);

    // put the data you sent from shiny into the billboard chart columns
    chart.load({
        columns: data_from_shiny,
        unload: true
    
    });
    
 });





