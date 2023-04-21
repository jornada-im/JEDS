# April 2023 Data Therapy: Geospatial Data Access
{:.no_toc}

**ONLINE - 20 April 2023:**  [Data Therapy](https://www.google.com/calendar/event?eid=YWlta3ZsZDUzbzdsM3VkbWVjN2ZwNDExbzBfMjAyMjA5MDhUMjEwMDAwWiBkczVtNnF0NTRsYm9xYm85Z3QxNDhzcjJjMEBn)

**Instructors:** Greg Maurer

 **Contents**

* TOC
{:toc}

## Questions we covered

### 1. Where are Jornada's geospatial data?

* The [Jornada LTER website spatial data catalog](https://lter.jornada.nmsu.edu/data-catalog/#uagb-tabs__tab3) has a limited selection of useful layers ad KMZ files. We will be expanding this over the next year.
* The Jornada Experimental Range (JER) geodatabase is the most reliable archive of the Jornada's spatial data layers that have been created and collected over decades of Jornada research. It is not public-facing, but if you request data (see below) someone can usually get you what you need. 
* The Jornada IM team's geodatabase is under development. For the most part it contains the same layers as are in the JER geodatabase, but we are working on improving metadata and usability of these layers. It is not public facing yet, but this will soon be the source of data for the Jornada website spatial data catalog.
* A few spatial datasets are published in the Environmental Data Initiative repository (EDI), and we are working on publishing more. You can search those through the [Jornada LTER website](https://lter.jornada.nmsu.edu/data-catalog/#uagb-tabs__tab1) or directly [at EDI](https://portal.edirepository.org).

### 2. Who should I ask for data?

Most of the time you should ask a Jornada data manager (<jornada.data@nmsu.edu>) and/or Amy Slaughter (<amalia.slaughter@usda.gov>), who maintains the JER geodatabase. This may be more efficient than searching our data catalogs.

### 3. How should I use the data?

Its up to you, but a few tools we can recommend:

* [QGIS](https://qgis.org/en/site/) is open-source, free, and powerful, and is installable on Mac, Windows, and Linux platforms. 
* [Google Earth Pro](https://www.google.com/earth/about/versions/) is also free and cross-platform, but a bit less powerful.
* [ArcGIS Desktop](https://www.esri.com/en-us/arcgis/products/arcgis-desktop/overview) or [ArcGIS Pro](https://www.esri.com/en-us/arcgis/products/arcgis-pro/overview) are great you have a license for them and use Windows (not available for Mac or Linux). Sometimes you can get a license through your institution. Many institutions also offer access to ArcGIS Online, a cloud-native GIS platform that integrates with both ArcGIS Desktop and Pro.
* For R users, you may install the R packages `terra` and `sf`, which are highly functional for raster and vector data, respectively.
* For python users, a good place to start might be GeoPandas, but there are many more options.

## Demonstrations

We demonstrated how to use the vector datasets from the Jornada LTER spatial data catalog in QGIS and R.

* In QGIS, one can copy the URL of a desired zip archive listed on the data catalog (such as 'https://lter.jornada.nmsu.edu/spatialfiles/JER_Fences.zip'), paste it into "Vector data" dialog of the "Open Data Source Manager", and click "Add", and QGIS will unpack the zip file and add the KMZ layers to the map.
* In R, there is no automated unpacking of zip files, so they must be downloaded and unzipped first. Then, the resulting KMZ files can be loaded as R objects using functions in `sf`, such as:

        fences <- sf::st_read('~/my_downloads/JER_fences/JER_Fences.kmz')

## Discussion on improving the spatial data catalog

We identified a number spatial datasets people at the Jornada would find useful to have around.

* Detailed aerial imagery is useful in lots of contexts. NAIP, Quickbird, and lots of other programs and platforms generate this kind of data, and the JER GIS office has archived a fair amount of it on local servers. Let us know what you need. Its big, and behind a VPN, so putting it on a Dropbox might be necessary.
* Most researchers have some need to make maps or other graphics with geospatial data. Background layers are really useful for this sometimes, and it doesn't make lots of sense to re-make them every time. Useful imagery mosaics, hillshades, or other nice map backgrounds would be nice to have easily available in a spatial catalog.
* Data resolution for things like DEMs and images keeps improving, so we need to update periodically with improved products.

## Survey

We're looking for feedback as we design a new spatial data catalog for the Jornada. [This survey](https://forms.gle/VQ5Nc7YUZv4Wb7go9) will help determine how that catalog will work and what data to include there.

## Some other helpful resources

* [Geospatial data in R Carpentries workshop (3 lessons)](https://datacarpentry.org/geospatial-workshop/)
