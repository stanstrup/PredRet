  document.domain = "predret.com";
  
  
  var userIDVariableName = parent.userID; 
  var userID = document.getElementById("userID"); 
  userID.value = userIDVariableName;
     
  var usernameVariableName = parent.username; 
  var username = document.getElementById("username"); 
  username.value = usernameVariableName;
  