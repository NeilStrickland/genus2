<?php

require_once('genus2.inc');

// Exit on notices or warnings

function errHandle($errNo, $errStr, $errFile, $errLine) {
 $msg = "$errStr in $errFile on line $errLine";
 if ($errNo == E_NOTICE || $errNo == E_WARNING) {
  throw new ErrorException($msg, $errNo);
 } else {
  echo $msg;
 }
}

set_error_handler('errHandle');

//////////////////////////////////////////////////////////////////////

ini_set('memory_limit',-1);

//////////////////////////////////////////////////////////////////////

function rm_rf($dir) {
 if (! $dir) { return(false); }
 if (! is_dir($dir)) { return(false); }
 
 $files = array_diff(scandir($dir), array('.', '..')); 

 foreach ($files as $file) {
  if (is_dir("$dir/$file")) {
   rm_rf("$dir/$file");
  } else {
   if (! unlink("$dir/$file")) {
    trigger_error("Could not delete $dir/$file",E_USER_ERROR);
    exit;
   }
  }
 }

 if (! rmdir($dir)) {
  trigger_error("Could not delete $dir",E_USER_ERROR);
  exit;
 }

 return true;
}

//////////////////////////////////////////////////////////////////////

function cp_r($src,$dst) { 
 $dir = opendir($src); 
 if (! is_dir($dst)) { mkdir($dst); }
 while(false !== ( $file = readdir($dir)) ) { 
  if (( $file != '.' ) && ( $file != '..' )) { 
   if ( is_dir($src . '/' . $file) ) { 
    cp_r($src . '/' . $file,$dst . '/' . $file); 
   } 
   else { 
    copy($src . '/' . $file,$dst . '/' . $file); 
   } 
  } 
 } 
 closedir($dir); 
} 

//////////////////////////////////////////////////////////////////////

function make_archives($src_dir,$dest_dir,$file) {
 global $map;

 $full_file = $dest_dir . '/' . $file;

 $phar = new PharData($full_file . '.tar');
 $phar->buildFromDirectory($src_dir);
 $phar->compress(Phar::GZ);
 $phar->stopBuffering();
 $phar = null;
 sleep(2);
 unlink($full_file . '.tar');
 rename($full_file . '.tar.gz',$full_file . '.tgz');

 $phar = new PharData($full_file . '.zip');
 $phar->buildFromDirectory($src_dir);

}

//////////////////////////////////////////////////////////////////////

$map->subdirs =
 array(
       '/latex',
       '/latex/tikz_includes',
       '/maple',
       '/maple/domain',
       '/maple/quadrature',
       '/maple/projective',
       '/maple/hyperbolic',
       '/maple/embedded',
       '/maple/embedded/roothalf',
       '/worksheets',
       '/doc',
       '/doc/classes',
       '/doc/maple',
       '/data',
       '/data/domain',
       '/data/quadrature',
       '/data/projective',
       '/data/hyperbolic',
       '/data/embedded',
       '/data/embedded/roothalf',
       '/images',
       '/plots',
       '/plots/thumbs'
       );

$map->files = array();

$map->files['doc'] =
 array(
       '/doc/index.html',
       '/doc/defs.html',
       '/doc/classes.html',
       '/doc/images.html'
       );

$map->files['latex'] =
 array(
       '/latex/genus2.tex',
       '/latex/genus2.bib',
       '/latex/genus2_talk.tex'
       );

$map->files['included_images'] = 
 array(
       '/images/EX.jpg',
       '/images/Omega.jpg',
       '/images/XOmega.jpg',
       '/images/curves_E.jpg',
       '/images/F4.jpg',
       '/images/pi_ring.jpg',
       '/images/delta_ring.jpg',
       '/images/zeta_ring.jpg',
       '/images/y_ring.jpg',
       '/images/E_to_S_u1.jpg',
       '/images/E_to_S_u2.jpg',
       '/images/E_to_S_u3.jpg',
       '/images/cromulent.png'
       );

$map->files['maple'] = 
 array(
       '/maple/Rn.mpl',
       '/maple/brent.mpl',
       '/maple/build_data.mpl',
       '/maple/check_all.mpl',
       '/maple/checks.mpl',
       '/maple/class.mpl',
       '/maple/cromulent.mpl',
       '/maple/genus2.mpl',
       '/maple/group.mpl',
       '/maple/groupoid.mpl',
       '/maple/latex.mpl',
       '/maple/nets.mpl',
       '/maple/plots.mpl',
       '/maple/talk_plots.mpl',
       '/maple/util.mpl',
       '/maple/projective/PX.mpl',
       '/maple/projective/galois.mpl',
       '/maple/projective/ellquot.mpl',
       '/maple/projective/picard_fuchs.mpl',
       '/maple/projective/PK_subfields.mpl',
       '/maple/projective/PX0.mpl',
       '/maple/projective/plots.mpl',
       '/maple/hyperbolic/Pi.mpl',
       '/maple/hyperbolic/HX.mpl',
       '/maple/hyperbolic/HX0.mpl',
       '/maple/hyperbolic/HP_table.mpl',
       '/maple/hyperbolic/H_to_P.mpl',
       '/maple/hyperbolic/P_to_H.mpl',
       '/maple/hyperbolic/geodesics.mpl',
       '/maple/hyperbolic/schwarz.mpl',
       '/maple/hyperbolic/plots.mpl',
       '/maple/hyperbolic/automorphic.mpl',
       '/maple/quadrature/quadrature.mpl',
       '/maple/domain/domain.mpl',
       '/maple/domain/domain_point.mpl',
       '/maple/domain/domain_edge.mpl',
       '/maple/domain/domain_face.mpl',
       '/maple/domain/grid.mpl',
       '/maple/embedded/EX.mpl',
       '/maple/embedded/invariants.mpl',
       '/maple/embedded/geometry.mpl',
       '/maple/embedded/curvature.mpl',
       '/maple/embedded/EX0.mpl',
       '/maple/embedded/extra_curves.mpl',
       '/maple/embedded/extra_vertices.mpl',
       '/maple/embedded/annular_charts.mpl',
       '/maple/embedded/disc_proj.mpl',
       '/maple/embedded/cayley.mpl',
       '/maple/embedded/barycentric.mpl',
       '/maple/embedded/E_domain.mpl',
       '/maple/embedded/E_quadrature.mpl',
       '/maple/embedded/homology.mpl',
       '/maple/embedded/plots.mpl',
       '/maple/embedded/roothalf/E_roothalf.mpl',
       '/maple/embedded/roothalf/cayley_surface.mpl',
       '/maple/embedded/roothalf/E_atlas.mpl',
       '/maple/embedded/roothalf/EH_atlas.mpl',
       '/maple/embedded/roothalf/crease.mpl',
       '/maple/embedded/roothalf/forms.mpl',
       '/maple/embedded/roothalf/rational.mpl',
       '/maple/embedded/roothalf/square_diffeo.mpl',
       '/maple/embedded/roothalf/zeta.mpl',
       '/maple/embedded/roothalf/sphere_quotients.mpl',
       '/maple/embedded/roothalf/better_sphere_quotients.mpl',
       '/maple/embedded/roothalf/E_to_S.mpl',
       '/maple/embedded/roothalf/torus_quotients.mpl',
       '/maple/embedded/roothalf/group64.mpl',
       '/maple/embedded/roothalf/galois.mpl',
       '/maple/embedded/roothalf/KR_subfields.mpl',
       '/maple/embedded/roothalf/plots.mpl',
       );

$map->files['checks'] =
 array(
       '/maple/brent_check.mpl',
       '/maple/cromulent_check.mpl',
       '/maple/group_check.mpl',
       '/maple/groupoid_check.mpl',
       '/maple/homology_check.mpl',
       '/maple/nets_check.mpl',
       '/maple/Rn_check.mpl',
       '/maple/embedded/EX_check.mpl',
       '/maple/embedded/annular_charts_check.mpl',
       '/maple/embedded/barycentric_check.mpl',
       '/maple/embedded/cayley_check.mpl',
       '/maple/embedded/curvature_check.mpl',
       '/maple/embedded/disc_proj_check.mpl',
       '/maple/embedded/extra_curves_check.mpl',
       '/maple/embedded/extra_vertices_check.mpl',
       '/maple/embedded/E_quadrature_check.mpl',
       '/maple/embedded/geometry_check.mpl',
       '/maple/embedded/homology_check.mpl',
       '/maple/embedded/invariants_check.mpl',
       '/maple/embedded/roothalf/E_roothalf_check.mpl',
       '/maple/embedded/roothalf/better_sphere_quotients_check.mpl',
       '/maple/embedded/roothalf/cayley_surface_check.mpl',
       '/maple/embedded/roothalf/crease_check.mpl',
       '/maple/embedded/roothalf/forms_check.mpl',
       '/maple/embedded/roothalf/galois_check.mpl',
       '/maple/embedded/roothalf/rational_check.mpl',
       '/maple/embedded/roothalf/sphere_quotients_check.mpl',
       '/maple/embedded/roothalf/square_diffeo_check.mpl',
       '/maple/embedded/roothalf/torus_quotients_check.mpl',
       '/maple/embedded/roothalf/zeta_check.mpl',
       '/maple/hyperbolic/HX_check.mpl',
       '/maple/hyperbolic/geodesics_check.mpl',
       '/maple/hyperbolic/Pi_check.mpl',
       '/maple/hyperbolic/schwarz_check.mpl',
       '/maple/projective/PX_check.mpl',
       '/maple/projective/ellquot_check.mpl',
       '/maple/projective/galois_check.mpl',
       '/maple/projective/picard_fuchs_check.mpl',
       '/maple/quadrature/quadrature_check.mpl',
       );

$map->files['all_maple'] =
 array_merge($map->files['maple'],$map->files['checks']);
 
$map->files['worksheets'] = 
 array(
       '/worksheets/genus2.mw',
       '/worksheets/genus2_pics.mw',
       '/worksheets/genus2_talk_pics.mw',
       '/worksheets/checks.mw',
       '/worksheets/check_all.mw',
       '/worksheets/text_check.mw',
       '/worksheets/build_data.mw',
       '/worksheets/build_data_toy.mw',
       );

$map->files['data'] =
 array(
       '/data/embedded/roothalf/EH_atlas.m',
       '/data/embedded/roothalf/E_to_S_map.m',
       '/data/embedded/roothalf/quadrature_frobenius_256a.m',
       '/data/embedded/roothalf/quadrature_frobenius_25a.m',
       '/data/embedded/roothalf/rational_grid_dunavant_19.m',
       '/data/embedded/roothalf/split_rational_grid_wx_30.m',
       '/data/embedded/roothalf/square_diffeo_E0_inverse.m',
       '/data/hyperbolic/automorphy_system.m',
       '/data/hyperbolic/HP_table.m',
       '/data/hyperbolic/H_to_P_map.m',
       '/data/hyperbolic/P_to_H_map.m',
       '/data/quadrature/dunavant_19.m',
       '/data/quadrature/wandzurat_xiao_30.m',
       );

$map->files['data_toy'] =
 array(
       '/data_toy/embedded/roothalf/EH_atlas.m',
       '/data_toy/embedded/roothalf/E_to_S_map.m',
       '/data_toy/embedded/roothalf/quadrature_frobenius_25a.m',
       '/data_toy/embedded/roothalf/rational_grid_dunavant_19.m',
       '/data_toy/embedded/roothalf/square_diffeo_E0_inverse.m',
       '/data_toy/hyperbolic/automorphy_system.m',
       '/data_toy/hyperbolic/HP_table.m',
       '/data_toy/hyperbolic/H_to_P_map.m',
       '/data_toy/hyperbolic/P_to_H_map.m',
       '/data_toy/quadrature/dunavant_19.m',
       '/data_toy/quadrature/wandzurat_xiao_30.m',
       );

$map->classes =
 array(
       'net',
       'triangle_quadrature_rule',
       'domain',
       'domain_point',
       'domain_edge',
       'domain_face',
       'grid',
       'E_point',
       'E_sample_point',
       'E_edge',
       'E_face',
       'E_grid',
       'E_quadrature_rule',
       'E_chart',
       'E_atlas',
       'EH_chart',
       'EH_atlas_edge',
       'EH_atlas',
       'E_to_S_map',
       'KR_subfield',
       'PK_subfield',
       'HP_table',
       'H_to_P_map',
       'P_to_H_chart',
       'P_to_H_map',
       'automorphy_system',
       );

//////////////////////////////////////////////////////////////////////

function make_arxiv() {
 global $map;
 
 rm_rf($map->arxiv_dir);
 mkdir($map->arxiv_dir);
 
 $tex = file_get_contents($map->main_dir . '/latex/genus2.tex');

 // All included tikz files have now been inlined, so the code below is
 // not needed, but we retain it in case we want to switch back.
 
 // $tex = preg_replace_callback(
 //  '/\\\\input{tikz_includes\/([^}]*)}/',
 //  function($matches) {
 //   global $map;
 //   return(file_get_contents($map->main_dir . '/latex/tikz_includes/' . $matches[1] . '.tex'));
 //  },
 //  $tex
 // );

 $tex = preg_replace('/{..\/images\/([^}]*)}/',
		     '{images/${1}}',
		     $tex);

 $tex = preg_replace('/\\\\bibliography{genus2}/',
		     file_get_contents($map->latex_dir . '/genus2.bbl'),
		     $tex);
 
 file_put_contents($map->arxiv_dir . '/genus2.tex',$tex);

 mkdir($map->arxiv_dir . '/images');
 foreach($map->files['included_images'] as $f) {
  copy($map->main_dir . $f,$map->arxiv_dir . $f);
 }

 $anc_dir = $map->arxiv_dir . '/anc';
 
 mkdir($anc_dir);
 mkdir($anc_dir . '/doc');
 mkdir($anc_dir . '/doc/maple');
 mkdir($anc_dir . '/images');
 mkdir($anc_dir . '/maple');
 mkdir($anc_dir . '/worksheets');

 mkdir($anc_dir . '/maple/domain');
 mkdir($anc_dir . '/maple/quadrature');
 mkdir($anc_dir . '/maple/projective');
 mkdir($anc_dir . '/maple/hyperbolic');
 mkdir($anc_dir . '/maple/embedded');
 mkdir($anc_dir . '/maple/embedded/roothalf');

 mkdir($anc_dir . '/doc/maple/domain');
 mkdir($anc_dir . '/doc/maple/quadrature');
 mkdir($anc_dir . '/doc/maple/projective');
 mkdir($anc_dir . '/doc/maple/hyperbolic');
 mkdir($anc_dir . '/doc/maple/embedded');
 mkdir($anc_dir . '/doc/maple/embedded/roothalf');
 mkdir($anc_dir . '/doc/classes');

 foreach($map->files['doc'] as $f) {
  copy($map->main_dir . $f,$anc_dir . $f);
 }
 
 foreach($map->files['all_maple'] as $f) {
  copy($map->main_dir . $f,$anc_dir . $f);
  copy($map->main_dir . '/doc/' . $f . '.html',
             $anc_dir . '/doc/' . $f . '.html');
 }

 foreach($map->classes as $c) {
  copy($map->main_dir . '/doc/classes/' . $c . '.html',
       $anc_dir . '/doc/classes/' . $c . '.html'
       );
 }
 
 foreach($map->files['worksheets'] as $f) {
  $b = basename($f);
  copy($map->main_dir . '/worksheets/small/' . $b,
       $anc_dir . '/worksheets/' . $b);
  
 }

 make_archives($map->arxiv_dir,$map->dist_dir,'arxiv');
}

//////////////////////////////////////////////////////////////////////

function make_thin_dist() {
 global $map;
 
 rm_rf($map->thin_dist_dir);
  
 mkdir($map->thin_dist_dir);
 mkdir($map->thin_dist_dir . '/doc');
 mkdir($map->thin_dist_dir . '/doc/maple');
 mkdir($map->thin_dist_dir . '/images');
 mkdir($map->thin_dist_dir . '/plots');
 mkdir($map->thin_dist_dir . '/latex');
 mkdir($map->thin_dist_dir . '/latex/tikz_includes');
 mkdir($map->thin_dist_dir . '/maple');
 mkdir($map->thin_dist_dir . '/worksheets');

 mkdir($map->thin_dist_dir . '/maple/domain');
 mkdir($map->thin_dist_dir . '/maple/quadrature');
 mkdir($map->thin_dist_dir . '/maple/projective');
 mkdir($map->thin_dist_dir . '/maple/hyperbolic');
 mkdir($map->thin_dist_dir . '/maple/embedded');
 mkdir($map->thin_dist_dir . '/maple/embedded/roothalf');

 mkdir($map->thin_dist_dir . '/doc/maple/domain');
 mkdir($map->thin_dist_dir . '/doc/maple/quadrature');
 mkdir($map->thin_dist_dir . '/doc/maple/projective');
 mkdir($map->thin_dist_dir . '/doc/maple/hyperbolic');
 mkdir($map->thin_dist_dir . '/doc/maple/embedded');
 mkdir($map->thin_dist_dir . '/doc/maple/embedded/roothalf');
 mkdir($map->thin_dist_dir . '/doc/classes');

 foreach($map->files['doc'] as $f) {
  copy($map->main_dir . $f,$map->thin_dist_dir . $f);
 }

 foreach($map->classes as $c) {
  copy($map->main_dir . '/doc/classes/' . $c . '.html',
       $map->thin_dist_dir . '/doc/classes/' . $c . '.html'
       );
 }

 foreach($map->files['included_images'] as $f) {
  copy($map->main_dir . $f,$map->thin_dist_dir . $f);
 }

 foreach($map->files['latex'] as $f) {
  copy($map->main_dir . $f,$map->thin_dist_dir . $f);
 }

 foreach($map->files['all_maple'] as $f) {
  copy($map->main_dir . $f,$map->thin_dist_dir . $f);
  copy($map->main_dir . '/doc/' . $f . '.html',
       $map->thin_dist_dir . '/doc/' . $f . '.html');
 }

 foreach($map->files['worksheets'] as $f) {
  $b = basename($f);
  copy($map->main_dir . '/worksheets/small/' . $b,
       $map->thin_dist_dir . '/worksheets/' . $b);
  
 }
}

//////////////////////////////////////////////////////////////////////

function make_fat_dist() {
 global $map;
 
 rm_rf($map->fat_dist_dir);
  
 mkdir($map->fat_dist_dir);
 mkdir($map->fat_dist_dir . '/doc');
 mkdir($map->fat_dist_dir . '/doc/maple');
 mkdir($map->fat_dist_dir . '/images');
 mkdir($map->fat_dist_dir . '/plots');
 mkdir($map->fat_dist_dir . '/latex');
 mkdir($map->fat_dist_dir . '/latex/tikz_includes');
 mkdir($map->fat_dist_dir . '/maple');
 mkdir($map->fat_dist_dir . '/worksheets');
 mkdir($map->fat_dist_dir . '/data');
 mkdir($map->fat_dist_dir . '/data_toy');

 mkdir($map->fat_dist_dir . '/maple/domain');
 mkdir($map->fat_dist_dir . '/maple/quadrature');
 mkdir($map->fat_dist_dir . '/maple/projective');
 mkdir($map->fat_dist_dir . '/maple/hyperbolic');
 mkdir($map->fat_dist_dir . '/maple/embedded');
 mkdir($map->fat_dist_dir . '/maple/embedded/roothalf');

 mkdir($map->fat_dist_dir . '/data/domain');
 mkdir($map->fat_dist_dir . '/data/quadrature');
 mkdir($map->fat_dist_dir . '/data/projective');
 mkdir($map->fat_dist_dir . '/data/hyperbolic');
 mkdir($map->fat_dist_dir . '/data/embedded');
 mkdir($map->fat_dist_dir . '/data/embedded/roothalf');

 mkdir($map->fat_dist_dir . '/data_toy/domain');
 mkdir($map->fat_dist_dir . '/data_toy/quadrature');
 mkdir($map->fat_dist_dir . '/data_toy/projective');
 mkdir($map->fat_dist_dir . '/data_toy/hyperbolic');
 mkdir($map->fat_dist_dir . '/data_toy/embedded');
 mkdir($map->fat_dist_dir . '/data_toy/embedded/roothalf');

 mkdir($map->fat_dist_dir . '/doc/maple/domain');
 mkdir($map->fat_dist_dir . '/doc/maple/quadrature');
 mkdir($map->fat_dist_dir . '/doc/maple/projective');
 mkdir($map->fat_dist_dir . '/doc/maple/hyperbolic');
 mkdir($map->fat_dist_dir . '/doc/maple/embedded');
 mkdir($map->fat_dist_dir . '/doc/maple/embedded/roothalf');
 mkdir($map->fat_dist_dir . '/doc/classes');

 foreach($map->files['doc'] as $f) {
  copy($map->main_dir . $f,$map->fat_dist_dir . $f);
 }

 foreach($map->classes as $c) {
  copy($map->main_dir . '/doc/classes/' . $c . '.html',
       $map->fat_dist_dir . '/doc/classes/' . $c . '.html'
       );
 }

 foreach($map->files['included_images'] as $f) {
  copy($map->main_dir . $f,$map->fat_dist_dir . $f);
 }

 foreach($map->files['latex'] as $f) {
  copy($map->main_dir . $f,$map->fat_dist_dir . $f);
 }

 foreach($map->files['all_maple'] as $f) {
  copy($map->main_dir . $f,$map->fat_dist_dir . $f);
  copy($map->main_dir . '/doc/' . $f . '.html',
       $map->fat_dist_dir . '/doc/' . $f . '.html');
 }

 foreach($map->files['worksheets'] as $f) {
  copy($map->main_dir . $f, $map->fat_dist_dir . $f);
 }

 foreach($map->files['data'] as $f) {
  copy($map->main_dir . $f, $map->fat_dist_dir . $f);
 }

 foreach($map->files['data_toy'] as $f) {
  copy($map->main_dir . $f, $map->fat_dist_dir . $f);
 }

 cp_r($map->images_dir,$map->fat_dist_dir . '/images'); 
 cp_r($map->plots_dir ,$map->fat_dist_dir . '/plots'); 
 cp_r($map->tikz_dir  ,$map->fat_dist_dir . '/latex/tikz_includes'); 

}

//////////////////////////////////////////////////////////////////////

function make_page_dir() {
 global $map;

 copy($map->latex_dir . '/genus2.pdf',$map->page_dir . '/genus2.pdf');

 $dir = $map->page_dir . '/doc';
 if (is_dir($dir)) { rm_rf($dir); }
 cp_r($map->doc_dir,$dir);

 $dir = $map->page_dir . '/images';
 if (is_dir($dir)) { rm_rf($dir); }
 cp_r($map->images_dir,$dir);

}
 
//////////////////////////////////////////////////////////////////////

function make_dist_archives() {
 global $map;

 make_archives($map->thin_dist_dir,$map->dist_dir,'genus2_thin');
 make_archives($map->fat_dist_dir ,$map->dist_dir,'genus2_fat');
}

//////////////////////////////////////////////////////////////////////

make_arxiv();
make_thin_dist();
make_fat_dist();
make_page_dir();
make_dist_archives();

?>
