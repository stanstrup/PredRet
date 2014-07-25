  document.domain = "predret.com";
  
  
  var userIDVariableName = parent.userID; 
  var userID = document.getElementById("userID"); 
  userID.value = userIDVariableName;
     
  var usernameVariableName = parent.username; 
  var username = document.getElementById("username"); 
  username.value = usernameVariableName;
  
  
  var user_logged_inVariableName = parent.user_logged_in; 
  var user_logged_in = document.getElementById("user_logged_in"); 
  user_logged_in.value = user_logged_inVariableName;