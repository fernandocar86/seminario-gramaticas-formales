{% assign code = include.code %}
{% assign language = include.language %}

``` {{ language }}
{{ code }}
```
{% assign nanosecond = "now" | date: "%N" %}
<textarea id="code{{ nanosecond }}" style="visibility:hidden">{{ code }}</textarea>
<button id="btn" onclick="copyCode( 'code{{ nanosecond }}' )" style="position:absolute; top:4px; right:4px;"
data-clipboard-target="#code{{ nanosecond }}">Copy</button>

<script>
function copyCode( queryId ) {
  var copyText = document.getElementById( queryId );
  navigator.clipboard.writeText(copyText.textContent);
  }
</script>
