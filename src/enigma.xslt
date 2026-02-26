<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:fn="http://www.w3.org/2005/xpath-functions">

<!-- ENIGMA CIPHER IN XSLT 2.0 -->
<!-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping -->
<!-- Usage: Apply to XML input like <enigma rotors="I II III" key="AAA" plugboard="" plaintext="HELLOWORLD"/> -->

<xsl:output method="text"/>

<!-- Rotor wirings -->
<xsl:variable name="ALPHA" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
<xsl:variable name="ROTOR_I"   select="'EKMFLGDQVZNTOWYHXUSPAIBRCJ'"/>
<xsl:variable name="ROTOR_II"  select="'AJDKSIRUXBLHWTMCQGZNPYFVOE'"/>
<xsl:variable name="ROTOR_III" select="'BDFHJLCPRTXVZNYEIWGAKMUSQO'"/>
<xsl:variable name="REFLECTOR" select="'YRUHQSLDPXNGOKMIEBFZCWVJAT'"/>
<xsl:variable name="NOTCH_I" select="16"/> <!-- Q -->
<xsl:variable name="NOTCH_II" select="4"/>  <!-- E -->
<xsl:variable name="NOTCH_III" select="21"/> <!-- V -->

<!-- char to index -->
<xsl:function name="fn:c2i" as="xs:integer">
  <xsl:param name="c" as="xs:string"/>
  <xsl:sequence select="string-length(substring-before($ALPHA, $c))"/>
</xsl:function>

<!-- index to char -->
<xsl:function name="fn:i2c" as="xs:string">
  <xsl:param name="i" as="xs:integer"/>
  <xsl:sequence select="substring($ALPHA, $i + 1, 1)"/>
</xsl:function>

<!-- Get rotor wiring string by number -->
<xsl:function name="fn:rotor-wiring" as="xs:string">
  <xsl:param name="num" as="xs:string"/>
  <xsl:choose>
    <xsl:when test="$num='I'"><xsl:sequence select="$ROTOR_I"/></xsl:when>
    <xsl:when test="$num='II'"><xsl:sequence select="$ROTOR_II"/></xsl:when>
    <xsl:when test="$num='III'"><xsl:sequence select="$ROTOR_III"/></xsl:when>
  </xsl:choose>
</xsl:function>

<!-- Get notch position -->
<xsl:function name="fn:notch" as="xs:integer">
  <xsl:param name="num" as="xs:string"/>
  <xsl:choose>
    <xsl:when test="$num='I'"><xsl:sequence select="$NOTCH_I"/></xsl:when>
    <xsl:when test="$num='II'"><xsl:sequence select="$NOTCH_II"/></xsl:when>
    <xsl:when test="$num='III'"><xsl:sequence select="$NOTCH_III"/></xsl:when>
  </xsl:choose>
</xsl:function>

<!-- Forward pass through rotor -->
<xsl:function name="fn:fwd" as="xs:integer">
  <xsl:param name="ch" as="xs:integer"/>
  <xsl:param name="wiring" as="xs:string"/>
  <xsl:param name="offset" as="xs:integer"/>
  <xsl:variable name="in" select="($ch + $offset) mod 26"/>
  <xsl:variable name="out" select="fn:c2i(substring($wiring, $in + 1, 1))"/>
  <xsl:sequence select="($out - $offset + 26) mod 26"/>
</xsl:function>

<!-- Backward pass through rotor -->
<xsl:function name="fn:bwd" as="xs:integer">
  <xsl:param name="ch" as="xs:integer"/>
  <xsl:param name="wiring" as="xs:string"/>
  <xsl:param name="offset" as="xs:integer"/>
  <xsl:variable name="in" select="($ch + $offset) mod 26"/>
  <xsl:variable name="inc" select="fn:i2c($in)"/>
  <xsl:variable name="pos" select="string-length(substring-before($wiring, $inc))"/>
  <xsl:sequence select="($pos - $offset + 26) mod 26"/>
</xsl:function>

<!-- Reflector pass -->
<xsl:function name="fn:reflect" as="xs:integer">
  <xsl:param name="ch" as="xs:integer"/>
  <xsl:sequence select="fn:c2i(substring($REFLECTOR, $ch + 1, 1))"/>
</xsl:function>

<!-- Plugboard swap -->
<xsl:function name="fn:plug" as="xs:integer">
  <xsl:param name="ch" as="xs:integer"/>
  <xsl:param name="pairs" as="xs:string"/>
  <xsl:choose>
    <xsl:when test="$pairs = ''"><xsl:sequence select="$ch"/></xsl:when>
    <xsl:otherwise>
      <xsl:variable name="c" select="fn:i2c($ch)"/>
      <xsl:variable name="tokens" select="tokenize($pairs, '-')"/>
      <xsl:variable name="result" select="$ch"/>
      <xsl:choose>
        <xsl:when test="some $t in $tokens satisfies substring($t,1,1)=$c">
          <xsl:sequence select="fn:c2i(substring($tokens[substring(.,1,1)=$c],2,1))"/>
        </xsl:when>
        <xsl:when test="some $t in $tokens satisfies substring($t,2,1)=$c">
          <xsl:sequence select="fn:c2i(substring($tokens[substring(.,2,1)=$c],1,1))"/>
        </xsl:when>
        <xsl:otherwise><xsl:sequence select="$ch"/></xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:function>

<!-- Encrypt one character given current rotor state, returns "CHAR,R,M,L" -->
<xsl:function name="fn:encrypt-char" as="xs:string">
  <xsl:param name="ch" as="xs:integer"/>
  <xsl:param name="rR" as="xs:string"/>
  <xsl:param name="rM" as="xs:string"/>
  <xsl:param name="rL" as="xs:string"/>
  <xsl:param name="oR" as="xs:integer"/>
  <xsl:param name="oM" as="xs:integer"/>
  <xsl:param name="oL" as="xs:integer"/>
  <xsl:param name="plugs" as="xs:string"/>

  <!-- Step rotors -->
  <xsl:variable name="midAtNotch" select="$oM = fn:notch($rM)"/>
  <xsl:variable name="rAtNotch" select="$oR = fn:notch($rR)"/>
  <xsl:variable name="newM" select="if ($midAtNotch or $rAtNotch) then ($oM + 1) mod 26 else $oM"/>
  <xsl:variable name="newL" select="if ($midAtNotch) then ($oL + 1) mod 26 else $oL"/>
  <xsl:variable name="newR" select="($oR + 1) mod 26"/>

  <xsl:variable name="wR" select="fn:rotor-wiring($rR)"/>
  <xsl:variable name="wM" select="fn:rotor-wiring($rM)"/>
  <xsl:variable name="wL" select="fn:rotor-wiring($rL)"/>

  <!-- Signal path -->
  <xsl:variable name="p" select="fn:plug($ch, $plugs)"/>
  <xsl:variable name="f3" select="fn:fwd($p, $wR, $newR)"/>
  <xsl:variable name="f2" select="fn:fwd($f3, $wM, $newM)"/>
  <xsl:variable name="f1" select="fn:fwd($f2, $wL, $newL)"/>
  <xsl:variable name="ref" select="fn:reflect($f1)"/>
  <xsl:variable name="b1" select="fn:bwd($ref, $wL, $newL)"/>
  <xsl:variable name="b2" select="fn:bwd($b1, $wM, $newM)"/>
  <xsl:variable name="b3" select="fn:bwd($b2, $wR, $newR)"/>
  <xsl:variable name="out" select="fn:plug($b3, $plugs)"/>

  <xsl:sequence select="concat(fn:i2c($out), ',', $newR, ',', $newM, ',', $newL)"/>
</xsl:function>

<!-- Recursive encrypt string -->
<xsl:function name="fn:encrypt-string" as="xs:string">
  <xsl:param name="text" as="xs:string"/>
  <xsl:param name="pos" as="xs:integer"/>
  <xsl:param name="rR" as="xs:string"/>
  <xsl:param name="rM" as="xs:string"/>
  <xsl:param name="rL" as="xs:string"/>
  <xsl:param name="oR" as="xs:integer"/>
  <xsl:param name="oM" as="xs:integer"/>
  <xsl:param name="oL" as="xs:integer"/>
  <xsl:param name="plugs" as="xs:string"/>
  <xsl:choose>
    <xsl:when test="$pos > string-length($text)"><xsl:sequence select="''"/></xsl:when>
    <xsl:otherwise>
      <xsl:variable name="ch" select="fn:c2i(substring($text, $pos, 1))"/>
      <xsl:variable name="result" select="fn:encrypt-char($ch, $rR, $rM, $rL, $oR, $oM, $oL, $plugs)"/>
      <xsl:variable name="parts" select="tokenize($result, ',')"/>
      <xsl:variable name="outChar" select="$parts[1]"/>
      <xsl:variable name="nR" select="xs:integer($parts[2])"/>
      <xsl:variable name="nM" select="xs:integer($parts[3])"/>
      <xsl:variable name="nL" select="xs:integer($parts[4])"/>
      <xsl:sequence select="concat($outChar, fn:encrypt-string($text, $pos+1, $rR, $rM, $rL, $nR, $nM, $nL, $plugs))"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:function>

<xsl:template match="/enigma">
  <xsl:variable name="rotors" select="tokenize(@rotors, ' ')"/>
  <xsl:variable name="key" select="@key"/>
  <xsl:variable name="plugboard" select="@plugboard"/>
  <xsl:variable name="plaintext" select="@plaintext"/>

  <xsl:variable name="rR" select="$rotors[3]"/>
  <xsl:variable name="rM" select="$rotors[2]"/>
  <xsl:variable name="rL" select="$rotors[1]"/>
  <xsl:variable name="oR" select="fn:c2i(substring($key,3,1))"/>
  <xsl:variable name="oM" select="fn:c2i(substring($key,2,1))"/>
  <xsl:variable name="oL" select="fn:c2i(substring($key,1,1))"/>

  <xsl:variable name="cipher" select="fn:encrypt-string($plaintext, 1, $rR, $rM, $rL, $oR, $oM, $oL, $plugboard)"/>

  <xsl:value-of select="concat('Enigma XSLT Implementation', '&#10;')"/>
  <xsl:value-of select="concat('Rotors: ', @rotors, '  Key: ', $key, '&#10;')"/>
  <xsl:value-of select="concat('Input:  ', $plaintext, '&#10;')"/>
  <xsl:value-of select="concat('Output: ', $cipher, '&#10;')"/>
</xsl:template>

</xsl:stylesheet>