<?xml version="1.0" encoding="UTF-8"?>


<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:blog="http://lojjic.net/blog"
				xmlns:jj="http://lojjic.net"
                xmlns:html="http://www.w3.org/1999/xhtml"
				xmlns:e="http://lojjic.net/emoticons"
                xmlns="http://www.w3.org/1999/xhtml"
				xmlns:office="http://openoffice.org/2000/office"
				xmlns:style="http://openoffice.org/2000/style"
				xmlns:text="http://openoffice.org/2000/text"
				xmlns:table="http://openoffice.org/2000/table"
				xmlns:xlink="http://www.w3.org/1999/xlink"
				xmlns:fo="http://www.w3.org/1999/XSL/Format"
				exclude-result-prefixes="xsl blog jj e html office style text table xlink fo">

	<!--xsl:import href="html-common.xsl" /-->

	<xsl:key name="styleDecl" match="//style:style" use="@style:name" />




	<!-- style-name attributes: -->
	<!--<xsl:template match="@text:style-name"><xsl:attribute name="class">ooo-<xsl:value-of select="." /></xsl:attribute></xsl:template>-->


	<!-- links: -->
	<xsl:template match="text:a"><a href="{@xlink:href}"><xsl:apply-templates /></a></xsl:template>

	

	<!-- headings: -->
	<xsl:template match="text:h">
		<xsl:element name="html:h{@text:level + 1}"><xsl:apply-templates /></xsl:element>
	</xsl:template>
	
	

	<!-- paragraph text: -->
	<xsl:template match="text:p"><p><xsl:apply-templates /></p></xsl:template>

	<!-- preformatted text - do some trickery to get consecutive preformatted paragraphs into the same <pre>: -->
	<xsl:template match="text:p[@text:style-name='Preformatted Text']">
		<xsl:if test="preceding-sibling::text:p[1]/@text:style-name != 'Preformatted Text'">
			<pre><xsl:call-template name="iteratePre"><xsl:with-param name="preElt" select="." /></xsl:call-template></pre>
		</xsl:if>
	</xsl:template>
	<xsl:template name="iteratePre">
		<xsl:param name="preElt" />
		<xsl:apply-templates select="$preElt/node()" />
		<xsl:variable name="nextElt" select="$preElt/following-sibling::text:p[1]" />
		<xsl:if test="$nextElt/@text:style-name = 'Preformatted Text'">
			<br />
			<xsl:call-template name="iteratePre"><xsl:with-param name="preElt" select="$nextElt" /></xsl:call-template>
		</xsl:if>
	</xsl:template>



	<!-- lists: -->
	<xsl:template match="text:unordered-list"><ul><xsl:apply-templates /></ul></xsl:template>
	<xsl:template match="text:ordered-list"><ol><xsl:apply-templates /></ol></xsl:template>
	<xsl:template match="text:list-item"><li><xsl:apply-templates /></li></xsl:template>


	
	
	<!-- tables: -->
	<xsl:template match="table:table"><table style="border-collapse:collapse;border: 1px solid ThreedShadow;"><xsl:apply-templates /></table></xsl:template>
	<!--<xsl:template match="table:table-column"><col><xsl:apply-templates /></col></xsl:template>-->
	<xsl:template match="table:table-header-rows"><thead><xsl:apply-templates /></thead></xsl:template>
	<xsl:template match="table:table-row"><tr><xsl:apply-templates /></tr></xsl:template>
	<xsl:template match="table:table-header-rows/table:table-row/table:table-cell"><th style="border: 1px solid ThreedShadow;"><xsl:apply-templates /></th></xsl:template>
	<xsl:template match="table:table-cell"><td style="border: 1px solid ThreedShadow;"><xsl:apply-templates /></td></xsl:template>




	<!-- remove lone paragraphs in table cells and list items: -->
	<xsl:template match="table:table-cell/text:p[count(../text:p)=1]"><xsl:apply-templates /></xsl:template>
	<xsl:template match="text:list-item/text:p[count(../text:p)=1]"><xsl:apply-templates /></xsl:template>




	<!-- ignore annotations: -->
	<xsl:template match="office:annotation"><!--<xsl:comment><xsl:apply-templates /></xsl:comment>--></xsl:template>




	<!-- empty elements, spaces: -->
	<xsl:template match="text:*[count(*|text())=0]"></xsl:template>
	<xsl:template match="text:line-break" priority="99"><br /></xsl:template><!-- line breaks -->

	<xsl:template match="text:s" priority="99"><!-- spaces - preserve for preformatted text -->
		<xsl:call-template name="iterateSpace"><xsl:with-param name="count" select="@text:c" /></xsl:call-template>
	</xsl:template>
	<xsl:template name="iterateSpace">
		<xsl:param name="count" />
		<xsl:text> </xsl:text>
		<xsl:if test="$count &gt; 1">
			<xsl:call-template name="iterateSpace"><xsl:with-param name="count" select="$count - 1" /></xsl:call-template>
		</xsl:if>
	</xsl:template>



	
	<!-- inline styles - munge them into structural elts, relying on conventions of emphasis: -->
	<xsl:template match="text:span">
		<xsl:variable name="props" select="key('styleDecl',@text:style-name)/style:properties" />
		<xsl:variable name="fontStyle" select="$props/@fo:font-style" />
		<xsl:variable name="fontWeight" select="$props/@fo:font-weight" />
		<xsl:variable name="underline" select="$props/@style:text-underline" />

		<xsl:choose>
			<xsl:when test="($fontStyle = 'italic') and ($fontWeight = 'bold') and ($underline and $underline != 'none')">
				<strong><em style="text-decoration:underline;"><xsl:apply-templates /></em></strong>
			</xsl:when>
			<xsl:when test="($fontStyle = 'italic') and ($fontWeight = 'bold')">
				<strong><em><xsl:apply-templates /></em></strong>
			</xsl:when>
			<xsl:when test="($fontStyle = 'italic') and ($underline and $underline != 'none')">
				<em style="text-decoration:underline"><xsl:apply-templates /></em>
			</xsl:when>
			<xsl:when test="($fontWeight = 'bold') and ($underline and $underline != 'none')">
				<strong style="text-decoration:underline; font-style:normal;"><xsl:apply-templates /></strong>
			</xsl:when>
			<xsl:when test="($fontStyle = 'italic')">
				<em><xsl:apply-templates /></em>
			</xsl:when>
			<xsl:when test="($fontWeight = 'bold')">
				<strong><xsl:apply-templates /></strong>
			</xsl:when>
			<xsl:when test="($underline and $underline != 'none')">
				<em style="text-decoration:underline; font-style:normal;"><xsl:apply-templates /></em>
			</xsl:when>
			<xsl:otherwise><xsl:apply-templates /></xsl:otherwise>
		</xsl:choose>
	</xsl:template>


</xsl:stylesheet>
