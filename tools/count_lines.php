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

$files = new stdClass();
$files->L = array();
$genus2_dir = dirname(__DIR__);
$maple_dir = $genus2_dir . '/maple';
ls_R($maple_dir,'',$files);

$n = 0;
foreach($files->L as $f) {
 $m = count(file($maple_dir . '/' . $f));
 $n += $m; 
 echo "$f : $m " . PHP_EOL;
}

echo PHP_EOL . PHP_EOL . "Total Maple: $n" . PHP_EOL;

$m = count(file($genus2_dir . '/latex/genus2.tex'));

echo "Total LaTeX: $m" . PHP_EOL;

?>
