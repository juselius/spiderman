$if(packages)$
    <table>
<tr>
<td>Program</td>
<td>Module</td>
<td>Default version</td>
<td>Description</td>
</tr>
    $packages:{
<tr>
    <td><a href="$it.infoUrl$">$it.displayName$</a></td>
    <td><a $it.defaultVersion.helpPageHref$>$it.moduleName$</a></td>
    <td><a href="$it.versionPageUrl$">$it.defaultVersionName$</a></td>
    <td>$it.description$</td>
</tr>
}$
    </table>
$elseif(versions)$
    <table>
    <tr>
    <td>Keywords:</td>
    <td>$keywords; separator=', '$</td>
    </tr>
    </table>
    
    <table>
    <tr><td>Version</td> <td>Module</td></tr>
    $versions:{
    <tr>
        <td>$it.version$</td>
        <td><a $it.helpPageHref$>$it.fullName$</a></td>
    </tr>
    }$
    </table>
$elseif(helptext)$
    <h4>List of <a href="$it.versionPageUrl$">available versions</a></h4>
    $helptext$
$endif$
