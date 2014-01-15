
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->
<p>Algorithms for Quantitative Pedology (AQP) is a collection of code, ideas, documentation, and examples wrapped-up into several <b>R</b> packages. The theory behind much of the code can be found in <a href="http://dx.doi.org/10.1016/j.cageo.2012.10.020">this Computers & Geosciences paper</a>. Links to project member contacts, code, and other information hosted by R-Forge can be found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. Worked examples along with discussion and application to soil survey work can be found on the <a href="http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56">CA Soil Resource Lab website</a>. AQP is very much a work in progress! If you are interested in contributing code, documentation, bug reports, or even scathing criticism, feel free to contact Dylan at debeaudette [at] ucdavis [dot] edu.</p>


<p><b>Tutorials</b>:
<ol>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp">SoilProfileCollection object introduction</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/dealing-with-bad-data.html?root=aqp">dealing with bad data</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/properties-by-bedrock.html?root=aqp">aggregate properties by bedrock kind</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-profile-dissimilarity.html?root=aqp">soil profile dissimilarity</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/vertical_vs_perpendicular.html?root=aqp">vertical vs. perpendicular horizon measurements</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/soilDB-Intro.html?root=aqp">getting soils data from USDA-NCSS databases</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/KSSL-demo.html?root=aqp">getting/comparing KSSL data</a></li>
<!-- <li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/RaCA-demo.html?root=aqp">getting RaCA data</a></li> -->
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/series-extent.html?root=aqp">getting, plotting, saving detailed soil series extent data (US-only)</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?root=aqp">gridded SSURRGO (gSSURGO) and SDA</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/component-relation-graph.html?root=aqp">Component Relation Graphs</a></li>
</ol>
</p>

<p><b>Manual Pages</b> c/o <a href="http://yihui.name/knitr/">knitr</a>:
<ol>
<li><a href="aqp-html-manual/index.html">aqp manual pages with figures</a></li>
<li><a href="soilDB-html-manual/index.html">soilDB manual pages with figures</a></li>
<li><a href="sharpshootR-html-manual/index.html">sharpshootR manual pages with figures</a></li>
</ol>
</p>


<p><b>Sample Figures</b></p>

<img src="brackets.png" border="0">

<img src="properties_by_mlra.png" border="0">

<img src="profile_compare-explanation.png" border="0">

<img src="hz_distinctness_demo.png" border="0">

<img src="hz_vs_site-and-hz_dend.png" border="0">

<img src="sanguinetti-pedons.png" border="0">

<img src="708x-report-demo.jpg" border="0">

<img src="munsell-soil_colors-LAB.png" border="0">

<img src="aqp_profile_plot_example_sjer.png" border="0">

<img src="aqp_profile_plot_example_sfrec.png" border="0">

<img src="MVO_morph_vs_depth_by_bedrock_kind-paired.jpg" border="0">

<img src="MVO_morph_vs_depth_by_bedrock_kind.png" border="0">

<img src="weighted_depth_func_aggregation.png" border="0">

<img src="categorical_depth_function.png" border="0">

<img src="classification_comparison.png" border="0">

<img src="dend_with_profiles-SJER.png" border="0">

<img src="dend_with_profiles.png" border="0">


</body>
</html>
