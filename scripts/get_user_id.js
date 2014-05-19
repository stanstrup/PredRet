document.domain = "10.0.100.99";

 var username = parent.username;
   var userID =   parent.userID;
   

document.write('Your username is '+username+' and user id is '+userID+'.');

  
Shiny.onInputChange("mydata", userID);
   