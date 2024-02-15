<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:cs="https://igsn.csiro.au/schemas/3.0"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xlink="http://www.w3.org/1999/xlink/namespace"

                xmlns:swes="http://www.opengis.net/swes/2.0"
                xmlns:sos="http://www.opengis.net/sos/2.0"
                xmlns:swe="http://www.opengis.net/swe/2.0"
                xmlns:sml="http://www.opengis.net/sensorml/2.0"
                xmlns:gml="http://www.opengis.net/gml/3.2"
                xmlns:sams="http://www.opengis.net/samplingSpatial/2.0"
                xmlns:sf="http://www.opengis.net/sampling/2.0"
                xmlns:gco="http://www.isotc211.org/2005/gco"
                xmlns:gmd="http://www.isotc211.org/2005/gmd"
                xmlns:skos="http://www.w3.org/2004/02/skos/core#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                exclude-result-prefixes="xs" version="2.0">

    <xsl:output method="html" doctype-system="about:legacy-compat" encoding="UTF-8" indent="yes"/>
    

    <xsl:strip-space elements="*" />

    <xsl:template match="/">
        <html lang="en">
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>

                <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <meta name="description"
                      content="Human readable version of a sample description from resources"/>
                <meta name="author" content="Alessandro Oggioni"/>
                <link rel="icon" href=".assets/img/icon/favicon.ico"/>

                <title>Sample description</title>
                <link rel="stylesheet" href="https://unpkg.com/leaflet@1.3.4/dist/leaflet.css"
                      integrity="sha512-puBpdR0798OZvTTbP4A8Ix/l+A4dHDD0DGqYW6RQ+9jxkRFclaxxQb/SJAWZfWAkuyeQUytO7+7N4QKrDh+drA=="
                      crossorigin=""/>

                <link href="./assets/css/font-awesome.min.css"
                      rel="stylesheet"/>

                <style type="text/css">
                    .tldate {
                    border: 1px solid #d4d4d4;
                    border-radius: 2px;
                    -webkit-box-shadow: 0 1px 6px rgba(0, 0, 0, 0.175);
                    box-shadow: 0 1px 6px rgba(0, 0, 0, 0.175);
                    display: block;
                    width: 200px;
                    background: #999999;
                    /*background: #414141;*/
                    /*border: 3px solid #212121;*/
                    color: #ededed;
                    margin: 0 auto;
                    padding: 3px 0;
                    font-weight: bold;
                    text-align: center;
                    /*-webkit-box-shadow: 0 0 11px rgba(0,0,0,0.35);*/
                    }

                    .span4 {
                    height: 100%;
                    overflow: auto;
                    }

                    #map {
                    position: absolute;
                    width: 100%;
                    height: 400px;
                    margin: 0;
                    padding: 0;
                    border: 1px solid #E5E5E5;
                    border-radius: 8px;
                    }

                    #mapRow {
                    height: 300px;
                    }

                    #map-outer {
                    height: 440px;
                    padding: 20px;
                    border: 2px solid #CCC;
                    margin-bottom: 20px;
                    background-color: #FFF
                    }

                    #map-container {
                    height: 400px
                    }

                    @media all and (max-width: 768px) {
                    #map-outer {
                    height: 650px
                    }
                    }


                    /*  bhoechie tab */
                    div.bhoechie-tab-container{
                    z-index: 10;
                    background-color: #ffffff;
                    padding: 0 !important;
                    border-radius: 4px;
                    -moz-border-radius: 4px;
                    border:1px solid #ddd;
                    margin-top: 20px;
                    margin-left: 50px;
                    -webkit-box-shadow: 0 6px 12px rgba(0,0,0,.175);
                    box-shadow: 0 6px 12px rgba(0,0,0,.175);
                    -moz-box-shadow: 0 6px 12px rgba(0,0,0,.175);
                    background-clip: padding-box;
                    opacity: 0.97;
                    filter: alpha(opacity=97);
                    }
                    div.bhoechie-tab-menu{
                    padding-right: 0;
                    padding-left: 0;
                    padding-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group{
                    margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group>a{
                    margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group>a .glyphicon,
                    div.bhoechie-tab-menu div.list-group>a .fa {
                    color: #5A55A3;
                    }
                    div.bhoechie-tab-menu div.list-group>a:first-child{
                    border-top-right-radius: 0;
                    -moz-border-top-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group>a:last-child{
                    border-bottom-right-radius: 0;
                    -moz-border-bottom-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group>a.active,
                    div.bhoechie-tab-menu div.list-group>a.active .glyphicon,
                    div.bhoechie-tab-menu div.list-group>a.active .fa{
                    background-color: #5A55A3;
                    background-image: #5A55A3;
                    color: #ffffff;
                    }
                    div.bhoechie-tab-menu div.list-group>a.active:after{
                    content: '';
                    position: absolute;
                    left: 100%;
                    top: 50%;
                    margin-top: -13px;
                    border-left: 0;
                    border-bottom: 13px solid transparent;
                    border-top: 13px solid transparent;
                    border-left: 10px solid #5A55A3;
                    }

                    div.bhoechie-tab-content{
                    background-color: #ffffff;
                    /* border: 1px solid #eeeeee; */
                    padding-left: 20px;
                    padding-top: 10px;
                    }

                    div.bhoechie-tab div.bhoechie-tab-content:not(.active){
                    display: none;
                    }
                </style>

                <script src="./assets/js/bootstrap-3.0.3.min.js" type="text/javascript"/>

                <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin="" />

                <!-- DataTables -->
                <link rel="stylesheet" href="./assets/css/bootstrap-3.3.7.min.css"/>
                <link rel="stylesheet" href="./assets/DataTables/datatables.css"/>

                <script type="text/javascript" src="./assets/js/jquery-1.12.4.js"/>
                <script type="text/javascript" src="./assets/js/jquery-1.10.16.dataTables.min.js"/>
                <script type="text/javascript" src="./assets/DataTables/datatables.js"/>

                <script type="text/javascript">
                    $(document).ready(function() {
                    $('#example').DataTable();
                    } );
                </script>

                <style>
                    .quote-card {
                    background: #fff;
                    color: #222222;
                    padding: 20px;
                    padding-left: 50px;
                    box-sizing: border-box;
                    box-shadow: 0 2px 4px rgba(34, 34, 34, 0.12);
                    position: relative;
                    overflow: hidden;
                    min-height: 120px;
                    }
                    .quote-card p {
                    font-size: 22px;
                    line-height: 1.5;
                    margin: 0;
                    max-width: 80%;
                    }
                    .quote-card cite {
                    font-size: 16px;
                    margin-top: 10px;
                    display: block;
                    font-weight: 200;
                    opacity: 0.8;
                    }
                    .quote-card:before {
                    font-family: Georgia, serif;
                    content: "“";
                    position: absolute;
                    top: 10px;
                    left: 10px;
                    font-size: 5em;
                    color: rgba(238, 238, 238, 0.8);
                    font-weight: normal;
                    }
                    .quote-card:after {
                    font-family: Georgia, serif;
                    content: "”";
                    position: absolute;
                    bottom: -110px;
                    line-height: 100px;
                    right: -32px;
                    font-size: 5em;
                    color: rgba(238, 238, 238, 0.8);
                    font-weight: normal;
                    }
                    @media (max-width: 640px) {
                    .quote-card:after {
                    font-size: 22em;
                    right: -25px;
                    }
                    }
                </style>

                <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"/>

            </head>

            <body>

                <div class="container">
                    <!-- Navbar -->
                    <nav class="navbar navbar-light" style="background-color: #F5F5F5;margin-top: 19px;">
                        <div class="container-fluid">
                            <div class="navbar-header">
                                <a class="navbar-brand" href="http://www.get-it.it" target="_blank">
                                    <img src="http://www.get-it.it/assets/img/getitLogos/logo1.svg" width="60" height="60" style="padding-top: 0px;margin-top: -19px;"/>
                                </a>
                            </div>
                            <ul class="nav navbar-nav navbar-right">
                                <li class="navbar-text">Sample landing page</li>
                            </ul>
                        </div>
                    </nav>
                    <!-- End Navbar -->

                    <!-- Central -->
                    <div class="row row-offcanvas row-offcanvas-right">

                        <div class="col-xs-12 col-sm-12 col-md-12 col-lg-12">
                            <!-- all -->
                            <div class="page-header">
                                <div class="row">
                                    <div class="col-xs-3 col-sm-3 col-md-3 col-lg-3" id="qrcode"/>
                                    <div class="col-xs-9 col-sm-9 col-md-9 col-lg-9">
                                        <xsl:call-template name="description"/>
                                    </div>
                                </div>
                            </div>

                            <div class="row">
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6" style="padding-right: 20px;">
                                    <xsl:call-template name="parameters" />
                                </div>
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6" style="padding-left: 20px;">
                                    <xsl:call-template name="position" />
                                </div>
                            </div>
                            <div class="row">
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6">
                                    <xsl:call-template name="curation" />
                                </div>
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6">
                                    <br/>
                                    <br/>
                                    <blockquote class="quote-card">
                                        <h3>How to cite</h3>
                                        <small>
                                            <xsl:call-template name="cite" />
                                        </small>
                                    </blockquote>
                                </div>
                            </div>
                        </div>
                        <!--/span-->

                    </div>
                    <!--/row-->
                    <!-- End Central -->

                    <!-- Site footer -->
                    <footer class="footer">
                        <hr/>
                        <div class="col-lg-6">
                            <p>2015 <a href="http://www.get-it.it" target="_blank">Geoinformation Enabling ToolkIT starterkit®</a></p>
                        </div>
                        <div class="col-lg-6">
                            <p>Icons by <a href="http://glyphicons.com/" target="_blank">Glyphicons</a></p>
                        </div>
                    </footer>
                    <!-- End Site footer -->

                </div>
                <!--/.container-->

                <!-- qrcod -->
                <script type="text/javascript">
                    $("#qrcode").qrcode({ text: "<xsl:value-of select="//cs:landingPage"/>",
                        render: "table",
                        width: 150,
                        height: 150
                    });
                </script>
            </body>
        </html>
    </xsl:template>

    <xsl:template name="cite">
        <xsl:variable name="landingPage" select="//cs:landingPage"/>
        <xsl:variable name="resID" select="//cs:resourceIdentifier"/>
        <xsl:value-of select="//cs:contributors/cs:contributor[@contributorType='//registry.it.csiro.au/def/isotc211/CI_RoleCode/owner']/cs:contributorName"/><xsl:text>. </xsl:text>
        <xsl:choose>
            <xsl:when test="//cs:timePeriod">
                <xsl:value-of select="substring(//cs:timePeriod/cs:end,1,4)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="substring(//cs:timeInstant,1,4)" />
            </xsl:otherwise>
        </xsl:choose><xsl:text>. </xsl:text>
        <b><xsl:value-of select="//cs:campaign"/><xsl:text> - </xsl:text><xsl:value-of select="//cs:resourceTitle"/><xsl:text>. </xsl:text></b>
        <xsl:value-of select="//cs:curationDetails/cs:curation/cs:curatingInstitution"/><xsl:text>. </xsl:text>
        <xsl:value-of select="//cs:campaign"/><xsl:text>. </xsl:text>
        <b><xsl:text>PID: </xsl:text><a href="{$resID}"><xsl:value-of select="//cs:resourceIdentifier"/></a></b><xsl:text>. </xsl:text>
    </xsl:template>

    <xsl:template name="description">
        <h1>
            <xsl:value-of select="//cs:resourceTitle" /> (PID: <xsl:value-of select="//cs:resourceIdentifier" />)<br/>
            <xsl:for-each select="//cs:logDate">
                <h3><b>Date of <xsl:value-of select="//cs:logDate/@eventType" /></b><xsl:text> </xsl:text><xsl:value-of select="//cs:logDate" /></h3>
            </xsl:for-each>
            <xsl:if
                    test="//cs:comments">
                <h4><small>
                    <b>Download: </b>
                    <xsl:choose>
                        <xsl:when test="//cs:isPublic/text()='true'">
                            <xsl:variable name="landingPage" select="//cs:landingPage"></xsl:variable>
                            <b>
                                <a role="button" class="btn btn-success btn-xs" href="{$landingPage}" download="{$landingPage}">
                                    <i class="fa fa-download" aria-hidden="true"></i>
                                    OPEN
                                </a>
                            </b>
                        </xsl:when>
                        <xsl:otherwise>
                            <a role="button" class="btn btn-danger btn-xs">
                                <i class="fa fa-download" aria-hidden="false"></i>
                                CLOSE
                            </a>
                        </xsl:otherwise>
                    </xsl:choose>
                    <!--br/><b>Licence: </b><a href="https://creativecommons.org/licenses/by/4.0/deed.en" target="_blank"><img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="20" /></a-->
                </small></h4>
            </xsl:if>
        </h1>
    </xsl:template>

    <xsl:template name="parameters">
        <h2>Resource information</h2>
        <!-- row -->
        <div class="row">
            <table id="resourceInfo" class="table table-striped table-bordered" width="100%" cellspacing="0">
                <!--thead>
                    <tr class="filters">
                        <th style="text-align: center;">Title (DEIMS website)</th>
                    </tr>
                </thead-->
                <tbody>
                    <!-- cs:comment -->
                    <tr>
                        <td>
                            <b>Description</b>
                        </td>
                        <td>
                            <xsl:value-of select="//cs:comments"/>
                        </td>
                    </tr>
                    <!-- cs:campaign -->
                    <tr>
                        <td>
                            <b>Campaign (activity from DEIMS-SDR)</b>
                        </td>
                        <td>
                            <xsl:variable name="activityLink" select="json-doc(concat('https://deims.org/api/activities/', substring-after(//cs:campaign, 'https://deims.org/activity/')))" />
                            <a href="{//cs:campaign}" target="_blank"><xsl:value-of select="$activityLink?title"/></a>
                        </td>
                    </tr>
                    <!-- cs:purpose -->
                    <tr>
                        <td>
                            <b>Purpose of collection</b>
                        </td>
                        <td>
                            <xsl:value-of select="//cs:purpose"/>
                        </td>
                    </tr>
                    <!-- cs:date -->
                    <xsl:for-each select="//cs:date">
                        <tr>
                            <td>
                                <b>Collecting date</b>
                            </td>
                            <td>
                                <xsl:choose>
                                    <xsl:when test="//cs:timePeriod">
                                        Time period of the collection from <xsl:value-of select="//cs:timePeriod/cs:start"/> to <xsl:value-of select="//cs:timePeriod/cs:end"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:value-of select="//cs:timeInstant" />
                                    </xsl:otherwise>
                                </xsl:choose>
                            </td>
                        </tr>
                    </xsl:for-each>
                </tbody>
            </table>
            <hr/>
            <table id="generalInfo" class="table table-striped table-bordered" width="100%" cellspacing="0">
                <tbody>
                    <!-- relatedResource -->
                    <xsl:for-each select="//cs:relatedResources/cs:relatedResource">
                        <xsl:choose>
                            <xsl:when test="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/') = 'LSID'">
                                <tr>
                                    <td>
                                        <b><xsl:value-of select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /></b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink" select="concat('http://www.lsid.info/', .)" />
                                        <xsl:value-of select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /> -
                                        <a href="{$relLink}" target="_blank"><xsl:value-of select="."/></a><br/>
                                        <xsl:value-of select="document($relLink)//rdf:RDF/rdf:Description/dc:type"/> - <a href="{$relLink}" target="_blank"><xsl:value-of select="document($relLink)//rdf:RDF/rdf:Description/dc:subject"/></a>
                                    </td>
                                </tr>
                            </xsl:when>
                            <xsl:when test="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/') = 'DOI'">
                                <tr>
                                    <td>
                                        <b><xsl:value-of select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /></b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink" select="concat('https://www.doi.org/', .)" />
                                        <xsl:value-of select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /> -
                                        <a href="{$relLink}" target="_blank"><xsl:value-of select="."/></a>
                                    </td>
                                </tr>
                            </xsl:when>
                            <xsl:otherwise>
                                <tr>
                                    <td>
                                        <b><xsl:value-of select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /></b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink" select="concat(., '.xml')" />
                                        <xsl:value-of select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')" /> -
                                        <a href="{$relLink}" target="_blank"><xsl:value-of select="."/></a>
                                    </td>
                                </tr>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:for-each>
                    <!-- resourceTypes -->
                    <xsl:for-each select="//cs:resourceTypes/cs:resourceType">
                        <tr>
                            <td>
                                <b>Resource type</b>
                            </td>
                            <td>
                                <xsl:variable name="typeLink" select="@xlink:href" />
                                <a href="{$typeLink}" target="_blank"><xsl:value-of select="."/></a>
                            </td>
                        </tr>
                    </xsl:for-each>
                    <!-- materialTypes -->
                    <xsl:for-each select="//cs:materialTypes/cs:materialType">
                        <tr>
                            <td>
                                <b>Material type</b>
                            </td>
                            <td>
                                <xsl:variable name="matLink" select="//cs:materialTypes/cs:materialType/@xlink:href" />
                                <a href="{$matLink}" target="_blank"><xsl:value-of select="//cs:materialTypes/cs:materialType"/></a>
                            </td>
                        </tr>
                    </xsl:for-each>
                    <!-- Classification -->
                    <xsl:for-each select="//cs:classifications/cs:classification">
                        <tr>
                            <td>
                                <b>Classification</b>
                            </td>
                            <td>
                                <xsl:value-of select="//cs:classifications/cs:classification"/>
                            </td>
                        </tr>
                    </xsl:for-each>
                    <!-- method -->
                    <tr>
                        <td>
                            <b>Method</b>
                        </td>
                        <td>
                            <xsl:variable name="methodLink" select="//cs:method/@methodURI" />
                            <a href="{$methodLink}" target="_blank"><xsl:value-of select="//cs:method/@methodURI"/></a>
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
    </xsl:template>

    <xsl:template name="curation">
        <div class="row">
            <xsl:if test="//cs:contributors">
                <!-- curation -->
                <h3>Curation by</h3>
                <xsl:for-each select="//cs:curationDetails/cs:curation">
                    <p>
                        <i class="glyphicon glyphicon-home"/>
                        <xsl:text> </xsl:text>
                        <!--<xsl:value-of select="./cs:contributorName" />--> <!--Substitute with ROR Institutuion Name-->
                        <!--<xsl:text> </xsl:text>-->
                        <xsl:choose>
                            <xsl:when test="(./cs:curatingInstitution/@institutionURI) and (contains(./cs:curatingInstitution/@institutionURI, 'ror.org'))">
                                <a href="{./cs:curatingInstitution/@institutionURI}" target="_blank">
                                    <img alt="ROR logo" src="https://raw.githubusercontent.com/ror-community/ror-logos/main/ror-icon-rgb.svg" height="24" />
                                    <!--<xsl:value-of select="./cs:curatingInstitution/@institutionURI"/>-->
                                </a>
                                <xsl:text> </xsl:text>
                                <xsl:variable name="rorLink" select="json-doc(concat('https://api.dev.ror.org/v2/organizations/', substring-after(./cs:curatingInstitution/@institutionURI, 'https://ror.org/')))" />                                
                                <xsl:value-of select="$rorLink?names?1?value"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <a href="{./cs:curatingInstitution/@institutionURI}" target="_blank">
                                    PID
                                </a>
                            </xsl:otherwise>
                        </xsl:choose>
                        <br/>
                    </p>
                </xsl:for-each>
            </xsl:if>
            <!-- contributor -->
            <xsl:if test="//cs:contributors">
                <xsl:for-each select="//cs:contributors/cs:contributor">
                    <h3>
                        Contributor - <xsl:value-of select="substring-after(./@contributorType, 'http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/')" />:
                    </h3>
                    <p>
                        <i class="glyphicon glyphicon-user"/>
                        <xsl:text> </xsl:text>
                        <xsl:choose>
                            <xsl:when test="(./cs:contributorIdentifier) and (contains(./cs:contributorIdentifier, 'orcid.org'))">
                                <a href="{./cs:contributorIdentifier}" target="_blank">
                                    <img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" height="16" width="16"/>
                                </a>
                            </xsl:when>
                            <xsl:otherwise>
                                <a href="{./cs:contributorIdentifier}" target="_blank">
                                    PID
                                </a>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text> </xsl:text>
                        <xsl:value-of select="./cs:contributorName" />
                        <xsl:text> </xsl:text>
                        <br/>
                    </p>
                    <!--<p>
                        <i class="glyphicon glyphicon-envelope"/>
                        <xsl:variable name="linkMailCont"
                                      select="./cs:contributorEmail"/>
                        <a href="mailto:{$linkMailCont}">
                            <xsl:text> </xsl:text>
                            <xsl:value-of
                                    select="$linkMailCont"/>
                            <br/>
                        </a>
                    </p>-->
                </xsl:for-each>
            </xsl:if>
        </div>
    </xsl:template>

    <xsl:template name="position">
        <xsl:variable name="locationURI" select="//cs:location/cs:locality/@localityURI"/>
        <xsl:variable name="locationIDAPI" select="json-doc(concat('https://deims.org/api/locations/', substring-after($locationURI, 'https://deims.org/locations/')))" />
        <xsl:variable name="sampledFURI" select="//cs:sampledFeatures/cs:sampledFeature/@sampledFeatureURI"/>
        <xsl:variable name="deimsIDAPI" select="json-doc(concat('https://deims.org/api/sites/', substring-after($sampledFURI, 'https://deims.org/')))" />
        <xsl:value-of select="$deimsIDAPI?title"/>
        <h2><a href="{//cs:location/cs:locality/@localityURI}" target="_blank"><xsl:value-of select="$locationIDAPI?properties?title"/></a> - Locality from DEIMS-SDR</h2>
        <!-- row -->
        <div class="row">
            <div id="map-container">
                <div id="map">
                    <!-- https://deims.org/geoserver/deims/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=deims:deims_sites_boundaries&outputFormat=text/javascript&CQL_FILTER=deimsid=%27https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe%27&amp;&amp;outputFormat=text/javascript&amp;format_options=callback:getJson&amp;srsName=epsg:4326 -->
                    <!-- map -->
                    <script type="text/javascript">
                        var popup;
                        var map;
                        function loadFOI() {
                        var owsrootUrl = "<xsl:value-of select="concat('https://deims.org/geoserver/deims/ows?service=WFS&amp;version=1.0.0&amp;request=GetFeature&amp;typeName=deims:deims_sites_boundaries&amp;outputFormat=text/javascript&amp;CQL_FILTER=deimsid=%27', $sampledFURI, '%27&amp;outputFormat=text/javascript&amp;format_options=callback:getJson&amp;srsName=epsg:4326')" />"
                        var URL = owsrootUrl;
                        var WFSLayer = null;
                        var ajax = $.ajax({
                          url : URL,
                          dataType : 'jsonp',
                          jsonpCallback : 'getJson',
                          success : function (response) {
                            console.log(response);
                            WFSLayer = L.geoJson(response, {
                              style: function (feature) {
                                return {
                                  stroke: false,
                                  fillColor: 'FFFFFF',
                                  fillOpacity: 0.3
                                };
                              },
                              onEachFeature: function (feature, layer) {
                                popupOptions = {maxWidth: 200};
                                layer.bindPopup('<a href="{$sampledFURI}"><xsl:value-of select="$deimsIDAPI?title"/></a> is the sampled feature of this sample', popupOptions);
                              }
                            }).addTo(map);
                            map.fitBounds(WFSLayer.getBounds());
                          }
                        });
                        }
                        
                        map = L.map('map');
                        popup = L.popup();
                        
                        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                        maxZoom: 19,
                        attribution: '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
                        }).addTo(map);
                        
                        loadFOI();
                    </script>
                </div>
            </div>
            <!-- /map-outer -->
        </div>
    </xsl:template>

</xsl:stylesheet>