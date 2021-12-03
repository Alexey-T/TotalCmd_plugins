<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fb="http://www.gribuser.ru/xml/fictionbook/2.0"  xmlns:xlink="http://www.w3.org/1999/xlink">
	<!-- author -->
	<xsl:template name="author">
		<xsl:value-of select="fb:first-name"/>
		<xsl:text disable-output-escaping="no">&#032;</xsl:text>
		<xsl:value-of select="fb:middle-name"/>&#032;
         <xsl:text disable-output-escaping="no">&#032;</xsl:text>
		<xsl:value-of select="fb:last-name"/>
		<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
	</xsl:template>

	<!-- secuence -->
	<xsl:template name="sequence">
		<LI/>
		<xsl:value-of select="@name"/>
		<xsl:if test="@number">
			<xsl:text disable-output-escaping="no">,&#032;#</xsl:text>
			<xsl:value-of select="@number"/>
		</xsl:if>
		<xsl:if test="fb:sequence">
			<UL>
				<xsl:for-each select="fb:sequence">
					<xsl:call-template name="sequence"/>
				</xsl:for-each>
			</UL>
		</xsl:if>
		<!--      <xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text> -->
	</xsl:template>
	
	<!-- toc template: body -->
	<xsl:template match="fb:body" mode="toc">
		<xsl:choose>
			<xsl:when test="not(@name) or @name != 'notes'"><xsl:apply-templates mode="toc" select="fb:section"/></xsl:when>
			<xsl:otherwise><br/><li><a href="#TOC_notes_{generate-id()}"><xsl:value-of select="$NotesTitle"/></a></li></xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- toc template: section -->
	<xsl:template match="fb:section" mode="toc">
   	<xsl:if test="fb:title | .//fb:section[count(ancestor::fb:section) &lt; $tocdepth]/fb:title">
   		<li>
	   		<xsl:apply-templates select="fb:title" mode="toc"/>
				<xsl:if test="(.//fb:section/fb:title) and (count(ancestor::fb:section) &lt; $tocdepth or $tocdepth=	4)">
					<UL><xsl:apply-templates select="fb:section" mode="toc"/></UL>
				</xsl:if>
			</li>
   	</xsl:if>
	</xsl:template>
	
	<!-- toc template: title -->
	<xsl:template match="fb:title" mode="toc">
		<a href="#TOC_{generate-id()}">
			<xsl:choose>
				<xsl:when test="$toccut &gt; 0">
					<xsl:value-of select="normalize-space(fb:p[1])"/>
				</xsl:when>
				<xsl:otherwise>
			      <xsl:for-each select="fb:title/fb:p">
						<xsl:if test="position()>1"><xsl:text> </xsl:text></xsl:if>
						<xsl:value-of select="normalize-space(.)"/>
			      </xsl:for-each>
				</xsl:otherwise>
			</xsl:choose>
		</a>
	</xsl:template>

	<!-- description -->
	<xsl:template match="fb:description">
		<xsl:apply-templates/>
	</xsl:template>
	
	<!-- section -->
	<xsl:template match="fb:section">
		<xsl:call-template name="preexisting_id"/>
		<xsl:apply-templates select="fb:title"/>
		<div><xsl:apply-templates select="fb:*[name()!='title']"/></div>
	</xsl:template>
	
	<!-- title -->
	<xsl:template match="fb:section/fb:title|fb:poem/fb:title">
		<xsl:choose>
			<xsl:when test="ancestor::fb:body/@name = 'notes' and not(following-sibling::fb:section)">
						<xsl:call-template name="preexisting_id"/>
						<xsl:for-each select="parent::fb:section">
							<xsl:call-template name="preexisting_id"/>
						</xsl:for-each><h1 align="center"><xsl:apply-templates/></h1>
			</xsl:when>
			<xsl:otherwise>
				<xsl:choose>
					<xsl:when test="count(ancestor::node()) &lt; 9">
						<xsl:element name="{concat('h',count(ancestor::node())-3)}">
							<xsl:attribute name="align">center</xsl:attribute>
							<a name="TOC_{generate-id()}"></a>
								<xsl:call-template name="preexisting_id"/>
							<xsl:apply-templates/>
						</xsl:element>
					</xsl:when>
					<xsl:otherwise>
						<xsl:element name="h6">
						    <xsl:attribute name="align">center</xsl:attribute>
							<xsl:call-template name="preexisting_id"/>
							<xsl:apply-templates/>
						</xsl:element>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- body/title -->
	<xsl:template match="fb:body/fb:title">
		<h1 align="center"><xsl:apply-templates/></h1>
	</xsl:template>

	<!-- title/p and the like -->
	<xsl:template match="fb:title/fb:p|fb:title-info/fb:book-title">
		<xsl:apply-templates/><xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
	</xsl:template>
	
	
	<!-- subtitle -->
	<xsl:template match="fb:subtitle">
		<xsl:call-template name="preexisting_id"/>
		<h3 align="center">
			<xsl:apply-templates/>
		</h3>
	</xsl:template>
	
	
	<!-- p -->
	<xsl:template match="fb:p"><xsl:call-template name="preexisting_id"/>&#160;&#160;&#160;&#160;&#160;&#160;<xsl:apply-templates/><xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text></xsl:template>

	<!-- strong -->
	<xsl:template match="fb:strong">
		<b><xsl:apply-templates/></b>
	</xsl:template>
	
	<!-- emphasis -->
	<xsl:template match="fb:emphasis">
		<i><xsl:apply-templates/></i>
	</xsl:template>
	
	<!-- style -->
	<xsl:template match="fb:style">
		<span class="{@name}"><xsl:apply-templates/></span>
	</xsl:template>
	
	<!-- empty-line -->
	<xsl:template match="fb:empty-line">
		&#160;<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
	</xsl:template>
	
	<!-- link -->
	<xsl:template match="fb:a">
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="@xlink:href"/></xsl:attribute>
			<xsl:attribute name="title">
				<xsl:choose>
					<xsl:when test="starts-with(@xlink:href,'#')"><xsl:value-of select="key('note-link',substring-after(@xlink:href,'#'))/fb:p"/></xsl:when>
					<xsl:otherwise><xsl:value-of select="key('note-link',@xlink:href)/fb:p"/></xsl:otherwise>
				</xsl:choose>
			</xsl:attribute>
			<xsl:choose>
				<xsl:when test="(@type) = 'note'">
					<sup>
						<xsl:apply-templates/>
					</sup>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>
	
	<!-- annotation -->
	<xsl:template name="annotation">
		<xsl:call-template name="preexisting_id"/>
		<h3 align="center">Annotation</h3>
		<xsl:apply-templates/>
	</xsl:template>
	
	<!-- epigraph -->
	<xsl:template match="fb:epigraph">
		<div align="right">
			<xsl:call-template name="preexisting_id"/>
			<i><xsl:apply-templates/></i>
		</div>
		<xsl:if test="name(./following-sibling::node()) = 'epigraph'"><br/></xsl:if>
		<br/>
	</xsl:template>
	
	<!-- epigraph/text-author -->
	<xsl:template match="fb:epigraph/fb:text-author">
			<b><i><xsl:apply-templates/></i></b>
	</xsl:template>
	
	<!-- cite -->
	<xsl:template match="fb:cite">
		<blockquote><div align="justify">
		<xsl:call-template name="preexisting_id"/>
		<xsl:apply-templates/></div>
		</blockquote>
	</xsl:template>
	
	<!-- cite/text-author -->
	<xsl:template match="fb:text-author">
		<b><xsl:apply-templates/></b><br/>
	</xsl:template>
	
	<!-- date -->
	<xsl:template match="fb:date">
		<xsl:choose>
			<xsl:when test="not(@value)">
				&#160;&#160;&#160;<xsl:apply-templates/>
				<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				&#160;&#160;&#160;<xsl:value-of select="@value"/>
				<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- poem -->
	<xsl:template match="fb:poem">
			<xsl:call-template name="preexisting_id"/>
			<xsl:apply-templates/>
	</xsl:template>
	
	<!-- stanza -->
	<xsl:template match="fb:stanza">
		&#160;<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
		<xsl:apply-templates/>
		&#160;<xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
	</xsl:template>

	<!-- v -->
	<xsl:template match="fb:v">
		<xsl:call-template name="preexisting_id"/>
		<i><xsl:apply-templates/></i><xsl:text disable-output-escaping="yes">&lt;br&gt;</xsl:text>
	</xsl:template>

	<!-- image - inline -->
	<xsl:template match="fb:p/fb:image|fb:v/fb:image|fb:td/fb:image|fb:subtitle/fb:image">
		<xsl:if test="$saveimages &gt; 0">
			<img border="0">
				<xsl:choose>
					<xsl:when test="starts-with(@xlink:href,'#')">
						<xsl:attribute name="src"><xsl:value-of select="substring-after(@xlink:href,'#')"/></xsl:attribute>
					</xsl:when>
					<xsl:otherwise>
						<xsl:attribute name="src"><xsl:value-of select="@xlink:href"/></xsl:attribute>
					</xsl:otherwise>
				</xsl:choose>
			</img>
		</xsl:if>
	</xsl:template>
	
	<!-- image - block -->
	<xsl:template match="fb:image">
		<xsl:if test="$saveimages &gt; 0">
			<div align="center">
				<img border="1">
					<xsl:choose>
						<xsl:when test="starts-with(@xlink:href,'#')">
							<xsl:attribute name="src"><xsl:value-of select="substring-after(@xlink:href,'#')"/></xsl:attribute>
						</xsl:when>
						<xsl:otherwise>
							<xsl:attribute name="src"><xsl:value-of select="@xlink:href"/></xsl:attribute>
						</xsl:otherwise>
					</xsl:choose>
				</img>
			</div>
		</xsl:if>
	</xsl:template>
	
	<!-- we preserve used ID's and drop unused ones -->
	<xsl:template name="preexisting_id">
		<xsl:variable name="i" select="@id"/>
		<xsl:if test="@id and //fb:a[@xlink:href=concat('#',$i)]"><a name="{@id}"/></xsl:if>
	</xsl:template>
	
	<!-- book generator -->
	<xsl:template name="DocGen">
		<xsl:for-each select="fb:body">
			<xsl:if test="position()!=1">
				<hr/>
			</xsl:if>
			<xsl:choose>
				<xsl:when test="@name = 'notes'"><h1 align="center"><a name="#TOC_notes_{generate-id()}"/><xsl:value-of 	select="$NotesTitle"/></h1></xsl:when>
				<xsl:when test="@name"><h1 align="center"><xsl:value-of select="@name"/></h1></xsl:when>
			</xsl:choose>
			<!-- <xsl:apply-templates /> -->
			<xsl:apply-templates/>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="CSS_Style_Screen">
		A { color : #0002CC }
		A:HOVER { color : #BF0000 }
		BODY {font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; text-align : justify }
		H1{ font-size : 160%; font-style : normal; font-weight : bold; text-align : left; text-transform : capitalize;  border : 1px solid Black;  background-color : #E7E7E7; text-transform : capitalize;  margin-left : 0px;  padding-left : 0.5em;  }
		H2{ font-size : 130%; font-style : normal; font-weight : bold; text-align : left; text-transform : capitalize;  background-color : #EEEEEE;  border : 1px solid Gray; text-transform : capitalize;  padding-left : 1em; }
		H3{ font-size : 110%; font-style : normal; font-weight : bold; text-align : left;  background-color : #F1F1F1;  border : 1px solid Silver; text-transform : capitalize;  padding-left : 1.5em;}
		H4{ font-size : 100%; font-style : normal; font-weight : bold; text-align : left   padding-left : 0.5em; text-transform : capitalize;  border : 1px solid Gray;  background-color : #F4F4F4;  padding-left : 2em;}
		H5{ font-size : 100%; font-style : italic; font-weight : bold; text-align : left; text-transform : capitalize;border : 1px solid Gray;  background-color : #F4F4F4;  padding-left : 2.5em;}
		H6{ font-size : 100%; font-style : italic; font-weight : normal; text-align : left; text-transform : capitalize;border : 1px solid Gray;  background-color : #F4F4F4;  padding-left : 2.5em;}
		SMALL{ font-size : 80% }
		BLOCKQUOTE{ margin : 0 1em 0.2em 4em }
		HR{ color : Black }
		UL{ padding-left : 1em; margin-left: 0}
		.epigraph{margin-right:5em; margin-left : 25%;}
		DIV{font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; text-align : justify}
	</xsl:template>
	<xsl:template name="CSS_Style_Print">
		A { color : #0002CC }
		A:HOVER { color : #BF0000 }
		BODY {font-family : "Times New Roman", Times, serif; text-align : justify }
		H1{ font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; font-size : 160%; font-style : normal; font-weight : bold; text-align : left; text-transform : capitalize }
		H2{ font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; font-size : 130%; font-style : normal; font-weight : bold; text-align : left; text-transform : capitalize }
		H3{ font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; font-size : 110%; font-style : normal; font-weight : bold; text-align : left }
		H4{ font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; font-size : 100%; font-style : normal; font-weight : bold; text-align : left }
		H5,H6{ font-family : Verdana, Geneva, Arial, Helvetica, sans-serif; font-size : 100%; font-style : italic; font-weight : normal; text-align : left; text-transform : uppercase }
		SMALL{ font-size : 80% }
		BLOCKQUOTE{ margin : 0 1em 0.2em 4em }
		HR{ color : Black }
		DIV{font-family : "Times New Roman", Times, serif; text-align : justify}
	</xsl:template>

</xsl:stylesheet>
