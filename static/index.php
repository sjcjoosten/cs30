<html><head>
  <!-- PHP
    <?php
    echo "set error reportin on\n";
    error_reporting(E_ALL & E_NOTICE);
    echo "set display errors on\n";
    ini_set("display_errors", '1');
    echo "require OAuth.php\n";
    require_once 'ims-blti/OAuth.php';
    echo "build request\n";
    $request = OAuthRequest::from_request();
    echo "get signature base string\n";
    $OAuth_base_string = $request->get_signature_base_string();
    echo "get signature\n";
    $oauth_signature = $request->get_parameter('oauth_signature');
    echo "done";
    ?> -->
  <title>CS30 Exercise and Test environment</title>
  <link href="https://fonts.googleapis.com/css?family=Nunito:300,400,700" rel="stylesheet">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js" type="text/javascript"></script>
  <script src="mathquill/mathquill.js" defer></script>
  <script src="js/sortable.js" defer></script>
  <script src="js/visnetwork.js" defer></script>
  <script>
  var postData = '<?php echo addslashes($OAuth_base_string) ?>';
  var signature = '<?php echo addslashes($oauth_signature) ?>';
  var userId = '<?php if (! (strlen($oauth_signature)>1)) {echo (bin2hex($_SERVER["UNIQUE_ID"]));};
   ?>';
  </script>
  <script src="js/cs30.js" defer></script>
  <!-- FONT -->
  <link href="//fonts.googleapis.com/css?family=Raleway:400,300,600" rel="stylesheet" type="text/css">
  <!-- CSS -->
  <link rel="stylesheet" href="mathquill/mathquill.css"/>
  <link rel="stylesheet" href="css/normalize.css">
  <link rel="stylesheet" href="css/skeleton.css">
  <meta name="viewport" content="width=device-width, initial-scale=1">
</head>
<!-- <?php
  if (false) {
    ?> -->
    <body class="top noPHP"><div class="container">This is a PHP page of which you're viewing the source code.
        A possible cause is that PHP is not enabled on the server from which you're seeing this.
        Without PHP, OAuth login cannot work (it uses 'POST' to send data, which JavaScript cannot read).
        You're probably best off asking a system administrator for help.</div>
    <!-- <?php
  } else {
    echo '--'.'!'.'><';
    echo 'body';
    // if we're not in Canvas, add some margin:
    if (strlen($oauth_signature)>1) echo ' class="inCanvas"'; else echo ' class="top"'; 
    echo '><!--';
  } ?>
-->
<noscript>
  You need to enable JavaScript to use this page.
  This page is meant to help you practice with interactive exercises.
  Those exercises need to be retrieved through json, so there really is no way to offer you a soft fallback.
</noscript>
<div class="container" id="header" style="display:none;margin-bottom:15px">
<div id="nav">
  <div id="nav-exercises" class="dropdown">
  <span class="u-full-width">Exercises</span>
  <div id="nav-exercise-list" class="dropdown-content"></div>
  </div>
</div>
<div id="login" style="margin: auto 0;"></div>
</div>
<div id="progress" style="display:none"></div>
<div id="done" style="display:none">Completed<br />Well done !</div>
<div class="overlay" id="splash" style="display:none">
</div>
<div class="container"><div class="u-pull-right" id="login2"></div></div>
<div class="row"></div>
<form onsubmit="return false;">
  <div class="container">
  <span id="serverCGI"></span>
  <div id="cards"></div>
  </div>
</form>
</body></html>