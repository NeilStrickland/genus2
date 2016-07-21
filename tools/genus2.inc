<?php

$map = new stdClass();

$map->genus2_dir     = dirname(dirname(__DIR__));
$map->main_dir       = $map->genus2_dir . '/main';

$map->images_dir     = $map->main_dir . '/images';
$map->plots_dir      = $map->main_dir . '/plots';
$map->latex_dir      = $map->main_dir . '/latex';
$map->tikz_dir       = $map->main_dir . '/latex/tikz_includes';
$map->maple_dir      = $map->main_dir . '/maple';
$map->worksheets_dir = $map->main_dir . '/worksheets';
$map->doc_dir        = $map->main_dir . '/doc';
$map->data_dir       = $map->main_dir . '/data';
$map->data_toy_dir   = $map->main_dir . '/data_toy';

$map->dist_dir       = $map->genus2_dir . '/dist';
$map->thin_dist_dir  = $map->dist_dir . '/genus2_thin';
$map->fat_dist_dir   = $map->dist_dir . '/genus2_fat';
$map->arxiv_dir      = $map->dist_dir . '/arxiv';

$map->page_dir       = $map->genus2_dir . '/page/genus2';

?>
