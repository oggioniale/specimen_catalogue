<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:xlink="http://www.w3.org/1999/xlink/namespace"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    xmlns:fn="http://www.w3.org/2005/xpath-functions"
    xmlns:dc="http://purl.org/dc/elements/1.1/" 
    xmlns:skos="http://www.w3.org/2004/02/skos/core#"
    xmlns:owl="http://www.w3.org/2002/07/owl#"
    xmlns:schema="http://schema.org/" 
    xmlns:dct="http://purl.org/dc/terms/"
    xmlns:omv="http://omv.ontoware.org/2005/05/ontology#" exclude-result-prefixes="xs" version="2.0">
    
    <xsl:output method="text" encoding="UTF-8"/>
    
    <xsl:template match="/">
        @prefix schema: &lt;http://schema.org/&gt; .
        @prefix adms: &lt;http://www.w3.org/ns/adms#&gt; .
        @prefix owl: &lt;http://www.w3.org/2002/07/owl#&gt; .
        @prefix org: &lt;http://www.w3.org/ns/org#&gt; .
        @prefix xls2rdf: &lt;https://xls2rdf.sparna.fr/vocabulary#&gt; .
        @prefix xsd: &lt;http://www.w3.org/2001/XMLSchema#&gt; .
        @prefix skosthes: &lt;http://purl.org/iso25964/skos-thes#&gt; .
        @prefix dcmitype: &lt;http://purl.org/dc/dcmitype/&gt; .
        @prefix rdfs: &lt;http://www.w3.org/2000/01/rdf-schema#&gt; .
        @prefix st: &lt;https://rdfdata.lteritalia.it/specimenType/&gt; .
        @prefix qb: &lt;http://purl.org/linked-data/cube#&gt; .
        @prefix dct: &lt;http://purl.org/dc/terms/&gt; .
        @prefix doap: &lt;http://usefulinc.com/ns/doap#&gt; .
        @prefix sh: &lt;http://www.w3.org/ns/shacl#&gt; .
        @prefix rdf: &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#&gt; .
        @prefix omv: &lt;http://omv.ontoware.org/2005/05/ontology#&gt; .
        @prefix dcat: &lt;http://www.w3.org/ns/dcat#&gt; .
        @prefix euvoc: &lt;http://publications.europa.eu/ontology/euvoc#&gt; .
        @prefix prov: &lt;http://www.w3.org/ns/prov#&gt; .
        @prefix foaf: &lt;http://xmlns.com/foaf/0.1/&gt; .
        @prefix dc: &lt;http://purl.org/dc/elements/1.1/&gt; .
        @prefix skos: &lt;http://www.w3.org/2004/02/skos/core#&gt; .
        @prefix skosxl: &lt;http://www.w3.org/2008/05/skos-xl#&gt; .
        &lt;https://rdfdata.lteritalia.it/specimenType/&gt; a skos:ConceptScheme;
            skos:prefLabel "LTER-Italy specimenType"@en;
            dct:title "LTER-Italy specimen type"@en;
            dct:description "Thesaurus for LTER-Italy about specimen type"@en;
            dct:creator &lt;https://orcid.org/0000-0002-7997-219X&gt;;
            dct:contributor &lt;https://ror.org/02wxw4x45&gt;;
            dct:license &lt;https://creativecommons.org/licenses/by/4.0/&gt;;
            owl:versionInfo "0.1.0"@en;
            dct:created "2024-08-30T11:00:00+00:00"@en;
            owl:ontolgoyIRI &lt;https://rdfdata.lteritalia.it/specimenType/&gt;;
            omv:acronym "SpecimenType"@en;
            omv:resourceLocator &lt;https://github.com/oggioniale/specimen_catalogue&gt;;
            omv:keywords &lt;http://vocabs.lter-europe.net/EnvThes/21672&gt;;
            dct:publisher &lt;https://orcid.org/0000-0002-7997-219X&gt;;
            dct:audience schema:Researcher;
            dct:language "en"@en;
            schema:includedInDataCatalog &lt;https://rdfdata.lteritalia.it/&gt;.
                                                                    
        <xsl:for-each select="//rdf:RDF/rdf:Description">
            <xsl:variable name="originalURI" select="./@rdf:about"/>
            <xsl:variable name="ID" select="substring-after($originalURI ,'http://vocabulary.odm2.org/specimentype/')"/>
            <xsl:value-of select="concat('st:', $ID)"/> a skos:Concept ;
            skos:prefLabel "<xsl:value-of select="./skos:prefLabel"/>"@en ;
            skos:definition "<xsl:value-of select="./skos:definition"/>"@en ;
            <xsl:choose>
                <xsl:when test="./skos:historyNote">dct:source <xsl:value-of select="concat('&lt;', ./skos:historyNote, '&gt; ;')" /></xsl:when>
                <xsl:otherwise>dct:source &lt;http://vocabulary.odm2.org/specimentype/&gt; ;</xsl:otherwise>
            </xsl:choose>
            owl:sameAs <xsl:value-of select="concat('&lt;', $originalURI, '&gt;')"/> ;
            dct:created "2024-08-30"^^xsd:date;
            skos:inScheme &lt;https://rdfdata.lteritalia.it/specimenType/&gt; ;
            skos:topConceptOf &lt;https://rdfdata.lteritalia.it/specimenType/&gt; .           
            
        </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>