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

function cmp($a,$b) {
 $i = substr_count($a,'/');
 $j = substr_count($b,'/');

 if ($i != $j) { return ($i - $j); }
 return strcmp(strtolower($a),strtolower($b));
}

$files = new stdClass();
$files->L = array();
$genus2_dir = dirname(__DIR__);
$maple_dir = $genus2_dir . '/maple';
ls_R($maple_dir,'',$files);

$check_files = array();

foreach($files->L as $f) {
 if (preg_match('/_check.mpl$/',$f)) {
  $check_files[] = $f;
 }
}

usort($check_files,'cmp');

foreach($check_files as $f) {
 echo $f . PHP_EOL;
}

?>
