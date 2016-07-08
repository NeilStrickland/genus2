<?php

function ls_R($top,$subdir,$files) {
 $dir = $top . '/' . $subdir;
 if ($handle = opendir($dir)) {
  while (false !== ($entry = readdir($handle))) {
   if ($entry != '.' && $entry != '..' && $entry != 'scratch' && $entry != 'old') {
    $f = $dir . "/" . $entry;
    $g = $subdir . "/" . $entry;
    if (is_dir($f)) {
     ls_R($top,$g,$files);
    } else {
     if (strlen($entry) >= 4 && substr($entry,-4) == '.mpl') {
      $files->L[] = $g;
     }
    }
   }
  }
  closedir($handle);
 }
}

function cmp_by_tag($a,$b) {
 return strcmp(strtolower($a->tag),strtolower($b->tag));
}

$files = new stdClass();
$files->L = array();
$genus2_dir = dirname(__DIR__);
$maple_dir = $genus2_dir . '/maple';
ls_R($maple_dir,'',$files);

$matches = array();
$defs = array();

$n = 0;
foreach($files->L as $f) {
 $lines = file($maple_dir . '/' . $f);
 $n = count($lines);
 $html = <<<HTML
<html>
 <head>
 </head>
 <body>
  <pre>
    
HTML;
 
 for ($i = 1; $i <= $n; $i++) {
  $line = $lines[$i-1];
  if (preg_match('/(.*)#@ ([^#]*)$/',$line,$matches)) {
   $x = new stdClass();
   $x->tag = trim($matches[2]," `");
   $x->full_file = $f;
   $x->dir = dirname($f);
   $x->file = basename($f);
   $x->line_number = $i;
   $defs[] = $x;
   $line = $matches[1] . '<span style="color:red">#@ ' . $matches[2] . '</span>';
  }
  $html .= '<a name="line_' . $i . '"></a>' . $line; 
 }

 $html .= <<<HTML
  </pre>
 </body>
</html>
    
HTML;

 file_put_contents('../doc/maple' . $f,$html);
}

usort($defs,'cmp_by_tag');

$list = '';

$list .= <<<HTML
<html>
 <head>
  <style type="text/css">
table { empty-cells:show; }

table.edged { 
 border-collapse: collapse;
}

table.edged td {
 padding: 3px 0.5em;
 margin-left: 3px;
 border: 1px solid #CCCCCC; 
}

td.letter {
 text-align: center;
 background-color: #4CAF50;
}

  </style>
 </head>
 <body>
  <table class="edged">
   <tr>

HTML;

for ($i = 0; $i < 26; $i++) {
 $a = chr(ord('A') + $i);
 $list .= <<<HTML
    <td><a href="#letter_{$a}">$a</a></td>
    
HTML;
}

$list .= <<<HTML
   </tr>
  </table>
  <table class="edged">

HTML;

$a = '';

foreach($defs as $d) {
 $t = $d->tag;
 $u = strtoupper(substr($t,0,1));
 if ($u != $a) {
  $a = $u;
  $list .= <<<HTML
   <tr>
    <td colspan="3" class="letter"><a name="letter_{$a}">$a</a></td>
   </tr>
   
HTML;
 }
 
 $list .= <<<HTML
   <tr>
    <td width="150"><tt>{$d->tag}</tt></td>
    <td width="250"><a href="../doc/maple{$d->full_file}#line_{$d->line_number}">{$d->full_file}</a></td>
    <td width="100">line {$d->line_number}</td>
   </tr>

HTML;
}

$list .= <<<HTML
  </table>
 </body>
</html>

HTML;

file_put_contents('../doc/defs.html',$list);

?>
