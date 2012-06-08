
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
<p> You can find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>.</p>

<p> You can find <strong></strong>extended examples + discussion</strong> <a href="http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56"><strong>here</strong></a>.</p>


<p>
Soils are routinely sampled and characterized according to genetic horizons,
resulting in data that are associated with principle dimensions: location (x,y), 
depth (z), and property space (p). The high dimensionality and grouped
nature of this type of data can complicate standard analysis, summarization, and visualization. The <tt>aqp</tt> (algorithms for quantitative pedology)
package was designed to support data-driven approaches to common soils-related tasks such as visualization, aggregation, and classification of soil
profile collections. In addition, we sought to advance the study of numerical soil classification by building on previously published methods within an
extensible and open source framework. Functions in the aqp package have
been successfully applied to studies involving several thousand soil profiles.
The stable version of the aqp package is hosted by CRAN (http://cran.r-project.org/web/packages/aqp), and the development version is hosted by
R-Forge (http://aqp.r-forge.r-project.org).
</p>


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
