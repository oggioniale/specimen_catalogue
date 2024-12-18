<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:cs="https://igsn.csiro.au/schemas/3.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:xlink="http://www.w3.org/1999/xlink/namespace"
    xmlns:swes="http://www.opengis.net/swes/2.0" xmlns:sos="http://www.opengis.net/sos/2.0"
    xmlns:swe="http://www.opengis.net/swe/2.0" xmlns:sml="http://www.opengis.net/sensorml/2.0"
    xmlns:gml="http://www.opengis.net/gml/3.2"
    xmlns:sams="http://www.opengis.net/samplingSpatial/2.0"
    xmlns:sf="http://www.opengis.net/sampling/2.0" xmlns:gco="http://www.isotc211.org/2005/gco"
    xmlns:gmd="http://www.isotc211.org/2005/gmd" xmlns:skos="http://www.w3.org/2004/02/skos/core#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:fn="http://www.w3.org/2005/xpath-functions"
    xmlns:dc="http://purl.org/dc/elements/1.1/" exclude-result-prefixes="xs" version="2.0">
    
    <xsl:output method="html" doctype-system="about:legacy-compat" encoding="UTF-8" indent="yes"/>

    <xsl:strip-space elements="*"/>

    <xsl:template match="/">
        <html lang="en">
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>

                <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <meta name="description"
                    content="Human readable version of a sample description from resources"/>
                <meta name="author" content="Alessandro Oggioni"/>
                <link rel="icon" href="https://www.lteritalia.it/wordpress/wp-content/uploads/2023/09/solo_foglia.png"/>

                <title>Sample description</title>
                <link rel="stylesheet" href="//www.get-it.it/objects/specimen/transformation_html/assets/css/leaflet.css"
                    integrity="sha512-puBpdR0798OZvTTbP4A8Ix/l+A4dHDD0DGqYW6RQ+9jxkRFclaxxQb/SJAWZfWAkuyeQUytO7+7N4QKrDh+drA=="
                    crossorigin=""/>

                <link href="//www.get-it.it/objects/specimen/transformation_html/assets/css/font-awesome.min.css" rel="stylesheet"/>

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
                    
                    @media all and (max-width : 768px) {
                        #map-outer {
                            height: 650px
                        }
                    }
                    
                    
                    /*  bhoechie tab */
                    div.bhoechie-tab-container {
                        z-index: 10;
                        background-color: #ffffff;
                        padding: 0 !important;
                        border-radius: 4px;
                        -moz-border-radius: 4px;
                        border: 1px solid #ddd;
                        margin-top: 20px;
                        margin-left: 50px;
                        -webkit-box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                        box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                        -moz-box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                        background-clip: padding-box;
                        opacity: 0.97;
                        filter: alpha(opacity=97);
                    }
                    div.bhoechie-tab-menu {
                        padding-right: 0;
                        padding-left: 0;
                        padding-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group {
                        margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a {
                        margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a .glyphicon,
                    div.bhoechie-tab-menu div.list-group > a .fa {
                        color: #5A55A3;
                    }
                    div.bhoechie-tab-menu div.list-group > a:first-child {
                        border-top-right-radius: 0;
                        -moz-border-top-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a:last-child {
                        border-bottom-right-radius: 0;
                        -moz-border-bottom-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a.active,
                    div.bhoechie-tab-menu div.list-group > a.active .glyphicon,
                    div.bhoechie-tab-menu div.list-group > a.active .fa {
                        background-color: #5A55A3;
                        background-image: #5A55A3;
                        color: #ffffff;
                    }
                    div.bhoechie-tab-menu div.list-group > a.active:after {
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
                    
                    div.bhoechie-tab-content {
                        background-color: #ffffff;
                        /* border: 1px solid #eeeeee; */
                        padding-left: 20px;
                        padding-top: 10px;
                    }
                    
                    div.bhoechie-tab div.bhoechie-tab-content:not(.active) {
                        display: none;
                    }</style>

                <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin=""/>

                <!-- DataTables -->
                <link rel="stylesheet"
                    href="//www.get-it.it/objects/specimen/transformation_html/assets/css/bootstrap-3.3.7.min.css"/>
                <link rel="stylesheet" href="//www.get-it.it/objects/specimen/transformation_html/assets/DataTables/datatables.css"/>

                <script type="text/javascript" src="//www.get-it.it/objects/specimen/transformation_html/assets/js/jquery-1.12.4.js"/>
                <script type="text/javascript" src="//www.get-it.it/objects/specimen/transformation_html/assets/js/jquery-1.10.16.dataTables.min.js"/>
                <script type="text/javascript" src="//www.get-it.it/objects/specimen/transformation_html/assets/DataTables/datatables.js"/>

                <script type="text/javascript">
                    $(document).ready(function () {
                        $('#example').DataTable();
                    });</script>

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
                    @media (max-width : 640px) {
                        .quote-card:after {
                            font-size: 22em;
                            right: -25px;
                        }
                    }</style>

                <script src="//www.get-it.it/objects/specimen/transformation_html/assets/js/jquery.qrcode.min.js"/>

                <script src="//www.get-it.it/objects/specimen/transformation_html/assets/js/bootstrap-3.0.3.min.js" type="text/javascript"/>
                
                <script src="//www.get-it.it/objects/specimen/transformation_html/assets/js/SaxonJS2.js"/>

            </head>

            <body>

                <div class="container">
                    <!-- Navbar -->
                    <nav class="navbar navbar-light"
                        style="background-color: #334155;margin-top: 19px;">
                        <div class="container-fluid">
                            <div class="navbar-header">
                                <a class="navbar-brand" href="https://www.lteritalia.it" target="_blank">
                                    <img src="https://www.lteritalia.it/wordpress/wp-content/uploads/LTER-IT-033-300x89.png"
                                        height="50"
                                        style="padding-top: 0px;margin-top: -19px;"/>
                                </a>
                            </div>
                            <ul class="nav navbar-nav navbar-right">
                                <span style="color: #94c5e5"><strong><li class="navbar-text">Sample landing page</li></strong></span>
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
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6"
                                    style="padding-right: 20px;">
                                    <xsl:call-template name="parameters"/>
                                </div>
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6"
                                    style="padding-left: 20px;">
                                    <xsl:call-template name="position"/>
                                </div>
                            </div>
                            <div class="row">
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6">
                                    <xsl:call-template name="curation"/>
                                </div>
                                <div class="col-xs-6 col-sm-6 col-md-6 col-lg-6">
                                    <br/>
                                    <br/>
                                    <blockquote class="quote-card">
                                        <h3>How to cite</h3>
                                        <small>
                                            <xsl:call-template name="cite"/>
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
                            <p>
                                <span style="color: #94c5e5"><strong>Contacts</strong></span><br/>
                                <strong>Secretariat: </strong>Via Roberto Cozzi, 53 20156 Milan (Italy)<br/>
                                <strong>Phone: </strong>+02 66173307<br/>
                                <strong>E-mail: </strong><a href="mailto:lteritaly@gmail.com" target="_blank">lteritaly@gmail.com</a>
                            </p>
                        </div>
                        <div class="col-lg-6">
                            <span style="color: #94c5e5"><strong>Useful links</strong></span><br/>
                            <a href="http://sparql.lteritalia.it/" target="_blank">SPARQL Endpoint</a>
                        </div>
                    </footer>
                    <!-- End Site footer -->

                </div>
                <!--/.container-->

                <!-- qrcode -->
                <script type="text/javascript">
                    $("#qrcode").qrcode({
                        text: "<xsl:value-of select="//cs:landingPage"/>",
                    render: "table",
                    width: 150,
                    height: 150
                });</script>

                <!-- copy text -->
                <script>
                    function copyText() {
                        /* Copy text into clipboard */
                        navigator.clipboard.writeText
                        ("@misc{<xsl:value-of select='//cs:resourceIdentifier'/>, author = {<xsl:for-each select='//cs:contributors/cs:contributor/cs:contributorName'> <xsl:if test='position() > 1'>, </xsl:if><xsl:value-of select='.'/></xsl:for-each>},title = {{<xsl:value-of select='//cs:resourceTitle'/>}},year = <xsl:choose><xsl:when test='//cs:timePeriod'><xsl:value-of select='substring(//cs:timePeriod/cs:end, 1, 4)'/></xsl:when><xsl:otherwise><xsl:value-of select='substring(//cs:timeInstant, 1, 4)'/></xsl:otherwise></xsl:choose>,publisher = {???},doi = {<xsl:value-of select='//cs:resourceIdentifier'/>},url = {<xsl:value-of select='//cs:landingPage'/>}}");
                    }
                </script>
            </body>
        </html>
    </xsl:template>

    <!--  Citation  -->
    <xsl:template name="cite">
        <xsl:variable name="landingPage" select="//cs:landingPage"/>
        <xsl:variable name="resID" select="//cs:resourceIdentifier"/>
        <xsl:for-each
            select="//cs:contributors/cs:contributor/cs:contributorName">
            <xsl:if test="position() > 1">, </xsl:if>
            <xsl:value-of select="."/>
        </xsl:for-each>
        <xsl:text>. </xsl:text>
        <xsl:choose>
            <xsl:when test="//cs:timePeriod">
                <xsl:value-of select="substring(//cs:timePeriod/cs:end, 1, 4)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="substring(//cs:timeInstant, 1, 4)"/>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:text>. </xsl:text>
        <xsl:value-of select="//cs:resourceTitle"/>
        <xsl:text>. </xsl:text>
        <b>
            <xsl:text>PID: </xsl:text>
            <a href="{$landingPage}">
                <xsl:value-of select="//cs:resourceIdentifier"/>
            </a>
        </b>
        <xsl:text>. </xsl:text>
        <br/>
        <a role="button" class="btn btn-danger btn-xs" onclick="copyText()">Copy BibTex</a>
    </xsl:template>
    
    <!--  Description  -->
    <xsl:template name="description">
        <h1>
            <xsl:value-of select="//cs:resourceTitle"/> (PID: <xsl:value-of
                select="//cs:resourceIdentifier"/>)<br/>
            <xsl:for-each select="//cs:logDate">
                <h3><b>Date of <xsl:value-of select="//cs:logDate/@eventType"
                        /></b><xsl:text> </xsl:text><xsl:value-of select="//cs:logDate"/></h3>
            </xsl:for-each>
            <xsl:if test="//cs:comments">
                <h4><small>
                        <b>Download: </b>
                        <xsl:choose>
                            <xsl:when test="//cs:isPublic/text() = 'true'">
                                <xsl:variable name="landingPage" select="//cs:landingPage"/>
                                <b>
                                    <a role="button" class="btn btn-success btn-xs"
                                        href="{$landingPage}" download="{$landingPage}">
                                        <svg xmlns="http://www.w3.org/2000/svg" width="16"
                                            height="16" fill="currentColor" class="bi bi-download"
                                            viewBox="0 0 16 16">
                                            <path
                                                d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                                            <path
                                                d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                                            />
                                        </svg> IGSN XML </a>
                                </b>
                            </xsl:when>
                            <xsl:otherwise>
                                <a role="button" class="btn btn-danger btn-xs">
                                    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"
                                        fill="currentColor" class="bi bi-sign-do-not-enter-fill"
                                        viewBox="0 0 16 16">
                                        <path
                                            d="M4.237 4.28h-.32v1.44h.32c.396 0 .582-.239.582-.718 0-.481-.188-.722-.582-.722m2.392.859v-.277c0-.413-.211-.617-.494-.617-.285 0-.495.204-.495.617v.277c0 .414.21.618.495.618.283 0 .494-.204.494-.618m4.163 0v-.277c0-.413-.211-.617-.494-.617-.285 0-.495.204-.495.617v.277c0 .414.21.618.495.618.283 0 .494-.204.494-.618m.006 5.828v-.694h.39c.231 0 .378.126.378.354 0 .225-.142.34-.387.34z"/>
                                        <path
                                            d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M3.584 6V4h.69c.596 0 .886.355.886.998S4.867 6 4.274 6zm3.382-1.135v.272c0 .566-.318.903-.83.903-.513 0-.833-.337-.833-.903v-.272c0-.569.32-.904.832-.904.513 0 .83.337.83.904Zm1.021-.305V6h-.319V4h.293l.933 1.436h.015V4h.319v2h-.291L8 4.56zm3.142.305v.272c0 .566-.318.903-.83.903-.513 0-.833-.337-.833-.903v-.272c0-.569.32-.904.832-.904.513 0 .83.337.83.904Zm.899-.58V6h-.333V4.285h-.584V4h1.503v.285zM5.413 11.72V12H4.165v-2h1.248v.28h-.917v.57h.862v.268h-.862v.602zm.572.28h-.32v-2h.294l.933 1.436h.014v-1.435h.32V12h-.292l-.936-1.44h-.013zm2.279 0H7.93v-1.715h-.584V10H8.85v.284h-.586zm1.953-.28V12H8.97v-2h1.248v.28H9.3v.57h.863v.268H9.3v.602zM11.235 10c.42 0 .674.244.674.616a.575.575 0 0 1-.368.56l.404.824h-.373l-.36-.769h-.414V12h-.328v-2zM3.5 7h9a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-9a.5.5 0 0 1-.5-.5v-1a.5.5 0 0 1 .5-.5"
                                        />
                                    </svg> CLOSE </a>
                            </xsl:otherwise>
                        </xsl:choose>
                    <xsl:text> </xsl:text>
                    <a role="button" class="btn btn-success btn-xs">
                        <svg xmlns="http://www.w3.org/2000/svg" width="16"
                            height="16" fill="currentColor" class="bi bi-download"
                            viewBox="0 0 16 16">
                            <path
                                d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                            <path
                                d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                            />
                        </svg> RDF XML </a>
                    <xsl:text> </xsl:text>
                    <a role="button" class="btn btn-success btn-xs">
                        <svg xmlns="http://www.w3.org/2000/svg" width="16"
                            height="16" fill="currentColor" class="bi bi-download"
                            viewBox="0 0 16 16">
                            <path
                                d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                            <path
                                d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                            />
                        </svg> Turtle </a>
                    <xsl:text> </xsl:text>
                    <a role="button" class="btn btn-success btn-xs">
                        <svg xmlns="http://www.w3.org/2000/svg" width="16"
                            height="16" fill="currentColor" class="bi bi-download"
                            viewBox="0 0 16 16">
                            <path
                                d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                            <path
                                d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                            />
                        </svg> LD + JSON </a>
                    </small></h4>
            </xsl:if>
        </h1>
    </xsl:template>

    <!--  Parameters  -->
    <xsl:template name="parameters">
        <h2>Sample information</h2>
        <!-- row -->
        <div class="row">
            <table id="resourceInfo" class="table table-striped table-bordered" width="100%"
                cellspacing="0">
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
                            <xsl:variable name="activityLink"
                                select="concat('&lt;activity&gt;https://deims.org/api/activities/', substring-after(//cs:campaign, 'https://deims.org/activity/'), '&lt;/activity&gt;')"/>
                            <a href="{//cs:campaign}" target="_blank">
                                <div id="log"></div>
                            </a>
                            
                            <script type="text/javascript">
                                const xml = `<xsl:value-of select="$activityLink" />`;
                                
                                const xslt = `&lt;xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                                    version="3.0"
                                    xmlns:xs="http://www.w3.org/2001/XMLSchema"
                                    exclude-result-prefixes="#all"
                                    expand-text="yes"&gt;
                                    
                                    &lt;xsl:template match="activity"&gt;
                                        &lt;xsl:copy&gt;{json-doc(.)?title}&lt;/xsl:copy&gt;
                                    &lt;/xsl:template&gt;
                                    
                                    &lt;xsl:mode on-no-match="shallow-copy"/&gt;
                                    
                                    &lt;xsl:template match="/" name="xsl:initial-template"&gt;
                                        &lt;xsl:next-match/&gt;
                                        &lt;xsl:comment&gt;Run with {system-property('xsl:product-name')} {system-property('xsl:product-version')} {system-property('Q{http://saxon.sf.net/}platform')}&lt;/xsl:comment&gt;
                                    &lt;/xsl:template&gt;
                                    
                                &lt;/xsl:stylesheet&gt;`;
                                
                                const result = SaxonJS.XPath.evaluate(`
                                  transform(map {
                                    'stylesheet-text' : $xslt,
                                    'source-node' : parse-xml($xml),
                                    'delivery-format' : 'serialized'
                                  })?output`,
                                  null,
                                  { params : { xml : xml, xslt : xslt } }
                                );
                                
                                document.getElementById('log').innerHTML = result;
                            </script>
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
                                    <xsl:when test="//cs:timePeriod"> Time period of the collection
                                        from <xsl:value-of select="//cs:timePeriod/cs:start"/> to
                                            <xsl:value-of select="//cs:timePeriod/cs:end"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:value-of select="//cs:timeInstant"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </td>
                        </tr>
                    </xsl:for-each>
                </tbody>
            </table>
            <hr/>
            <table id="generalInfo" class="table table-striped table-bordered" width="100%"
                cellspacing="0">
                <tbody>
                    <!-- relatedResource -->
                    <xsl:for-each select="//cs:relatedResources/cs:relatedResource">
                        <xsl:choose>
                            <xsl:when
                                test="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/') = 'LSID'">
                                <tr>
                                    <td>
                                        <b>
                                            <xsl:value-of
                                                select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                            />
                                        </b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink"
                                            select="concat('http://www.lsid.info/', .)"/>
                                        <xsl:value-of
                                            select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                        /> - <a href="{$relLink}" target="_blank"><xsl:value-of
                                                select="."/></a><br/>
                                        <xsl:value-of
                                            select="document($relLink)//rdf:RDF/rdf:Description/dc:type"
                                        /> - <a href="{$relLink}" target="_blank"><xsl:value-of
                                                select="document($relLink)//rdf:RDF/rdf:Description/dc:subject"
                                            /></a>
                                    </td>
                                </tr>
                            </xsl:when>
                            <xsl:when
                                test="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/') = 'DOI'">
                                <tr>
                                    <td>
                                        <b>
                                            <xsl:value-of
                                                select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                            />
                                        </b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink"
                                            select="concat('https://www.doi.org/', .)"/>
                                        <xsl:value-of
                                            select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                        /> - <a href="{$relLink}" target="_blank"><xsl:value-of
                                                select="."/></a>
                                    </td>
                                </tr>
                            </xsl:when>
                            <xsl:otherwise>
                                <tr>
                                    <td>
                                        <b>
                                            <xsl:value-of
                                                select="substring-after(@relationType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                            />
                                        </b>
                                    </td>
                                    <td>
                                        <xsl:variable name="relLink" select="concat(., '.xml')"/>
                                        <xsl:value-of
                                            select="substring-after(@relatedResourceIdentifierType, 'http://pid.geoscience.gov.au/def/voc/igsn-codelists/')"
                                        /> - <a href="{$relLink}" target="_blank"><xsl:value-of
                                                select="."/></a>
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
                                <xsl:variable name="typeLink" select="."/>
                                <!-- TODO move the skos vocabs of medium in fuseky1 because the http cannot be called from https -->
                                <a href="{$typeLink}" target="_blank">
                                    <!--<xsl:value-of select="document(concat('http://vocabulary.odm2.org/api/v1/specimentype/', substring-after($typeLink, 'http://vocabulary.odm2.org/specimentype/'), '/?format=skos'))//rdf:RDF/rdf:Description/skos:prefLabel"/>-->
                                    <xsl:value-of select="$typeLink"/>
                                </a>
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
                                <xsl:variable name="matLink" select="."/>
                                <!-- TODO move the skos vocabs of medium in fuseky1 because the http cannot be called from https -->
                                <a href="{$matLink}" target="_blank">
                                    <!--<xsl:value-of select="document(concat('http://vocabulary.odm2.org/api/v1/medium', substring-after($matLink, 'medium'), '/?format=skos'))//rdf:RDF/rdf:Description/skos:prefLabel"/>-->
                                    <xsl:value-of select="$matLink"/>
                                </a>
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
                            <xsl:variable name="methodLink" select="//cs:method/@methodURI"/>
                            <a href="{$methodLink}" target="_blank">
                                <xsl:value-of select="//cs:method/@methodURI"/>
                            </a>
                        </td>
                    </tr>
                    <!-- sampler -->
                    <tr>
                        <td>
                            <b>Sampler</b>
                        </td>
                        <td>
                            <xsl:value-of select="//cs:method"/>
                            <!-- TODO when the sampler will be stored in triplestore find the content of //cs:method in skos and put here the link to the page of vocabulary -->
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
    </xsl:template>

    <!--  Curation  -->
    <xsl:template name="curation">
        <div class="row">
            <xsl:if test="//cs:curationDetails/cs:curation">
                <!-- curation -->
                <h3>Curation by</h3>
                <xsl:for-each select="//cs:curationDetails/cs:curation">
                    <xsl:variable name="componentsPosition" select="position()" />
                    <xsl:variable name="rorLink"
                        select="concat('&lt;organization&gt;https://api.ror.org/v2/organizations/', substring-after(./cs:curatingInstitution/@institutionURI, 'https://ror.org/'), '&lt;/organization&gt;')"/>
                    <xsl:choose>
                        <xsl:when
                            test="contains(./cs:curatingInstitution/@institutionURI, 'ror.org')">
                            <a href="{./cs:curatingInstitution/@institutionURI}" target="_blank">
                                <img alt="ROR logo"
                                    src="//www.get-it.it/objects/specimen/transformation_html/assets/img/icon/ror-icon-rgb.svg"
                                    height="24"/>
                                <xsl:text> </xsl:text>
                                <b id="logRor{$componentsPosition}"></b>
                            </a>
                            <script type="text/javascript">
                                const xmlRor<xsl:value-of select="$componentsPosition"/> = `<xsl:value-of select="$rorLink" />`;
                                
                                const xsltRor<xsl:value-of select="$componentsPosition"/> = `&lt;xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                                version="3.0"
                                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                                exclude-result-prefixes="#all"
                                expand-text="yes"&gt;
                                
                                &lt;xsl:template match="organization"&gt;
                                &lt;xsl:copy&gt;{json-doc(.)?names?2?value}&lt;/xsl:copy&gt;
                                &lt;/xsl:template&gt;
                                
                                &lt;xsl:mode on-no-match="shallow-copy"/&gt;
                                
                                &lt;xsl:template match="/" name="xsl:initial-template"&gt;
                                &lt;xsl:next-match/&gt;
                                &lt;xsl:comment&gt;Run with {system-property('xsl:product-name')} {system-property('xsl:product-version')} {system-property('Q{http://saxon.sf.net/}platform')}&lt;/xsl:comment&gt;
                                &lt;/xsl:template&gt;
                                
                                &lt;/xsl:stylesheet&gt;`;
                                
                                const resultRor<xsl:value-of select="$componentsPosition"/> = SaxonJS.XPath.evaluate(`
                                  transform(map {
                                    'stylesheet-text' : $xsltRor<xsl:value-of select="$componentsPosition"/>,
                                    'source-node' : parse-xml($xmlRor<xsl:value-of select="$componentsPosition"/>),
                                    'delivery-format' : 'serialized'
                                  })?output`,
                                  null,
                                  { params : { xmlRor<xsl:value-of select="$componentsPosition"/> : xmlRor<xsl:value-of select="$componentsPosition"/>, xsltRor<xsl:value-of select="$componentsPosition"/> : xsltRor<xsl:value-of select="$componentsPosition"/> } }
                                );
                                
                                document.getElementById('logRor<xsl:value-of select="$componentsPosition"/>').innerHTML = resultRor<xsl:value-of select="$componentsPosition"/>;
                            </script>
                        </xsl:when>
                        <xsl:otherwise>
                            <a href="{./cs:curatingInstitution/@institutionURI}" target="_blank"> PID </a>
                        </xsl:otherwise>
                    </xsl:choose>
                    <br/>
                </xsl:for-each>
            </xsl:if>
            <!-- contributor -->
            <xsl:if test="//cs:contributors">
                <xsl:for-each select="//cs:contributors/cs:contributor">
                    <h3> Contributor - <xsl:value-of
                            select="substring-after(./@contributorType, 'http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/')"
                        />: </h3>
                    <p>
                        <xsl:text> </xsl:text>
                        <xsl:choose>
                            <xsl:when
                                test="(./cs:contributorIdentifier) and (contains(./cs:contributorIdentifier, 'orcid.org'))">
                                <a href="{./cs:contributorIdentifier}" target="_blank">
                                    <img
                                        src="//www.get-it.it/objects/specimen/transformation_html/assets/img/icon/orcid_16x16.png"
                                        height="16" width="16"/>
                                    <xsl:text> </xsl:text>
                                    <xsl:value-of select="./cs:contributorName"/>
                                </a>
                            </xsl:when>
                            <xsl:otherwise>
                                <a href="{./cs:contributorIdentifier}" target="_blank"> PID
                                    <xsl:text> </xsl:text>
                                    <xsl:value-of select="./cs:contributorName"/>
                                </a>
                            </xsl:otherwise>
                        </xsl:choose>
                        <br/>
                    </p>
                </xsl:for-each>
            </xsl:if>
        </div>
    </xsl:template>

    <!--  Position  -->
    <xsl:template name="position">
        <xsl:variable name="locationURI" select="//cs:location/cs:locality/@localityURI"/>
        <xsl:variable name="locationIDURL"
            select="concat('https://deims.org/api/locations/', substring-after($locationURI, 'https://deims.org/locations/'))"/>
        <xsl:variable name="locationDEIMSID"
            select="concat('&lt;location&gt;https://deims.org/api/locations/', substring-after($locationURI, 'https://deims.org/locations/'), '&lt;/location&gt;')"/>
        <xsl:variable name="sampledFURI"
            select="//cs:sampledFeatures/cs:sampledFeature/@sampledFeatureURI"/>
        <xsl:variable name="deimsIDAPI"
            select="concat('&lt;sampledFeature&gt;https://deims.org/api/sites/', substring-after($sampledFURI, 'https://deims.org/'), '&lt;/sampledFeature&gt;')"/>
        <xsl:variable name="deimsIDurl"
            select="concat('https://deims.org/api/sites/', substring-after($sampledFURI, 'https://deims.org/'))"/>
        <h4>The sampling locations is:</h4> 
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
			(async () => {
                            const deimsIDurl = '<xsl:value-of select="$deimsIDurl"/>';
			    const responseSite = await fetch(deimsIDurl);
			    const jsonSite = await responseSite.json();
                            console.log('jsonSite', jsonSite);
                            var owsrootUrl = "<xsl:value-of select="concat('https://deims.org/geoserver/deims/ows?service=WFS&amp;version=1.0.0&amp;request=GetFeature&amp;typeName=deims:deims_sites_boundaries&amp;outputFormat=text/javascript&amp;CQL_FILTER=deimsid=%27', $sampledFURI, '%27&amp;outputFormat=text/javascript&amp;format_options=callback:getJson&amp;srsName=epsg:4326')"/>"
                            var URL = owsrootUrl;
                            var WFSLayer = null;
                            var ajax = $.ajax({
                                url: URL,
                                dataType: 'jsonp',
                                jsonpCallback: 'getJson',
                                success: function (response) {
                                    console.log(response);
                                    WFSLayer = L.geoJson(response, {
                                        style: function (feature) {
                                            return {
                                                stroke: true,
                                                color: '#FFFFFF',
                                                fillColor: '#EAA75E',
                                                fillOpacity: 0.8
                                            };
                                        },
                                        onEachFeature: function (feature, layer) {
                                            popupOptions = {
                                                maxWidth: 200
                                            };
                                            layer.bindPopup(`Site: <a href="{$sampledFURI}">${jsonSite.title}</a>`, popupOptions);
                                        }
                                    }).addTo(map);
                                    map.fitBounds(WFSLayer.getBounds());
                                }
                            });

                            map = L.map('map');
                            popup = L.popup();

                            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                                maxZoom: 19,
                                attribution: '© <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
                            }).addTo(map);

                            const locationIDURL = '<xsl:value-of select="$locationIDURL"/>';
			                const response = await fetch(locationIDURL);
			                const geojsonFeature = await response.json();

			                console.log('geojsonFeature', geojsonFeature);
				
                            L.geoJSON(geojsonFeature, {
                            	    style: function (feature) {
                                	    return {
                                    		    stroke: true,
                                    		    color: '#336600'
                                	    };
                            	    },
                            	    onEachFeature: function (feature, layer) {
                                	    popupOptions = {
                                    		    maxWidth: 200
                                	    };
                                	    layer.bindPopup(`Location: <a href="{//cs:location/cs:locality/@localityURI}" target="_blank">${feature.properties.title}</a><br/>Type: ${feature.properties.locationType.label}`, popupOptions);
                            	    }
                            }).addTo(map)

			              })();
                        }
                        loadFOI();
                    </script>
                </div>
            </div>
            <!-- /map-outer -->
        </div>
    </xsl:template>

</xsl:stylesheet>
