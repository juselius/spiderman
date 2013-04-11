$if(packages)$
    <table>
<tr>
<td>Program</td>
<td>Module</td>
<td>Default version</td>
<td>Keywords</td>
<td>Description</td>
</tr>
    $packages:{
<tr>
    <td><a href="$it.infoUrl$">$it.displayName$</a></td>
    <td><a href="$it.defaultVersion.helpPageUrl$$ext$">$it.moduleName$</a></td>
    <td><a href="$it.versionPageUrl$$ext$">$it.defaultVersionName$</a></td>
    <td>$it.keywords; separator=', '$</td>
    <td>$it.description$</td>
</tr>
}$
    </table>
$elseif(helptext)$
    $helptext$
$elseif(versions)$
    <table>
    $versions:{
    <tr>
        <td>$it.version$</td>
        <td><a href="$it.helpPageUrl$$ext$">$it.fullName$</a></td>
    </tr>
    }$
    </table>
$endif$
