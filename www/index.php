
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
<p>Algorithms for Quantitative Pedology (AQP) is a collection of code, ideas, documentation, and examples wrapped-up into several <b>R</b> packages. The theory behind much of the code can be found in <a href="http://dx.doi.org/10.1016/j.cageo.2012.10.020">this Computers & Geosciences paper</a>. Links to project member contacts, code, and other information hosted by R-Forge can be found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. Worked examples along with discussion and application to soil survey work can be found on the <a href="http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56">CA Soil Resource Lab website</a>. AQP is very much a work in progress! If you are interested in contributing code, documentation, bug reports, or even scathing criticism, feel free to contact Dylan at debeaudette [at] ucdavis [dot] edu.</p> AQP is a collaborative effort, funded in part by the Kearney Foundation of Soil Science (2009-2011) and USDA-NRCS (2011-current).

<p><b>Presentations</b>:
<ol>
<li><a href="http://soilmap2-1.lawr.ucdavis.edu/dylan/presentations/2015-morphometrics-ghl/presentation.html">2015 Digital Soil Morphometrics - Aggregate representation of genetic soil horizons via proportional-odds logistic regression</a></li>
<li><a href="http://soilmap2-1.lawr.ucdavis.edu/dylan/presentations/2015-morphometrics-aqp/presentation.html">2015 Digital Soil Morphometrics - Algorithms for Quantitative Pedology: a toolkit for digital soil morphometrics</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/ghl-aggregation.html?root=aqp">Soil Data Aggregation</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/aqp-esd-dsm.html?root=aqp">AQP, DSM, and ESD</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/AQP-num_soil_classification.pdf?root=aqp">Numerical Classification with AQP</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/AQP-soilDB-beaudette.pdf?root=aqp">AQP and SoilDB Demo</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/aqp_UseR-2011.pdf?root=aqp">2011 UseR AQP Talk</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/presentations/poster-aqp-pedometrics-2011.pdf?root=aqp">2011 Pedometrics</a></li>
</ol>
</p>

<p><b>Tutorials</b>:
<ol>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-intro.html?root=aqp">SoilProfileCollection object introduction</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/dealing-with-bad-data.html?root=aqp">dealing with bad data</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/profile-summary.html?root=aqp">aggregate properties by taxon name</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/properties-by-bedrock.html?root=aqp">aggregate properties by bedrock kind</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/aqp-profile-dissimilarity.html?root=aqp">soil profile dissimilarity</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/vertical_vs_perpendicular.html?root=aqp">vertical vs. perpendicular horizon measurements</a></li>

<!-- <li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/RaCA-demo.html?root=aqp">getting RaCA data</a></li> -->
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/series-extent.html?root=aqp">getting, plotting, saving detailed soil series extent data (US-only)</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/component-relation-graph.html?root=aqp">Component Relation Graphs</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/diagnostic-property-plot.html?root=aqp">A Novel Display of Categorical Data</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/OSD-dendrogram.html?root=aqp">OSD Dendrogram</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/hz-transition-probabilities.html?root=aqp">Horizon Transition Probabilities</a></li>

<li>USDA-NRCS Data Sources<ol>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/soilDB-Intro.html?root=aqp">getting soils data from USDA-NCSS databases</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/SDA-tutorial.html?root=aqp">Querying the Soil Data Access web service</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/KSSL-demo.html?root=aqp">getting/comparing KSSL data</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/fetchNASIS-mini-tutorial.html?root=aqp">Loading NASIS pedon data</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/NASIS-component-data.html?root=aqp">Loading NASIS component data</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/export-points-from-NASIS.html?root=aqp">Export NASIS pedon data to SHP file</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/export-points-from-NASIS-to-Google-Earth.html?root=aqp">Export NASIS pedon data to Google Earth</a></li>

<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/gSSURGO-SDA.html?root=aqp">gridded SSURRGO (gSSURGO) and SDA</a></li>
</ol></li>

<li>Pedon Data Aggregation<ol>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/aqp/gen-hz-assignment.html?root=aqp">Assignment of generalized horizon labels</a></li>
<li>Computing range in characteristics by generalized horizon label</li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/aggregate-soil-color.html?root=aqp">Soil color aggregation ideas</a></li>
<li>Estimation of most-likely horizonation</li>
<li>Sample Reports<ul>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/example-reports/dunstone.html?root=aqp">Dunstone Pedon Summary</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/example-reports/loafercreek.html?root=aqp">Loafercreek Pedon Summary</a></li>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/example-reports/amador.html?root=aqp">Amador Pedon Summary</a></li>
</ol></li>

<li>Spatial Data Aggregation<ol>
<li><a href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/sharpshootR/GIS-summary-by-MU.html?root=aqp">Sampling Raster Data Sources</a></li>
</ol></li>


</ol></li>

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

<img src="tp-example.png" border="0">

<img src="MO-soils-aqp-demo.png" border="0">

<img src="soil-series-color-signature.png" border="0">

<img src="aggregate-summary-as-profiles.jpg" border="0">

<img src="mineralking-set1-elevation.png" border="0">

<img src="miami-and-related.png" border="0">

<img src="OSD-dend.png" border="0">

<img src="diagnosticFeatures.png" border="0">

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
