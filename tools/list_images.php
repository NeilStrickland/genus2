<?php

$handle = opendir("../images");
$images = array();

while (false !== ($entry = readdir($handle))) {
 if (strlen($entry) >= 4 && substr($entry,-4) == '.jpg') {
  $images[] = $entry;
 }
}

sort($images);

$w = 100;
$n = 6;

$html = <<<HTML
<html>
 <head>
  <title>Images</title>
  <style type="text/css">
table { empty-cells:show; }

table.edged { 
 border-collapse: collapse;
}

table.edged td {
 padding: 3px 0.5em;
 margin-left: 3px;
 border: 1px solid #CCCCCC; 
 vertical-align: top;
}
  </style>
 </head>
 <body>
  <h1>Images</h1>
  <br/>
  <table class="edged">

HTML;

$i = 0;

foreach($images as $f) {
 if ($i == 0) {
  $html .= <<<HTML
   <tr>

HTML;
 }

 $g = substr($f,0,strlen($f) - 4);

 $html .= <<<HTML
    <td><img width="$w" src="../images/$f"/><br/><a href="../images/$f">$g</a></td>

HTML;
 $i = $i+1;
 if ($i == $n) {
  $html .= <<<HTML
   </tr>

HTML;
  $i = 0;
 }
}

if ($i > 0) {
 $html .= <<<HTML
   </tr>

HTML;
}

$html .= <<<HTML
  </table>
 </body>

HTML;

file_put_contents("../doc/images.html",$html);

?>
