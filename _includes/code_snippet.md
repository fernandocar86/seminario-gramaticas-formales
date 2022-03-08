{% assign code = include.code %}
{% assign language = include.language %}

``` {{ language }}
{{ code }}
```
{% assign nanosecond = "now" | date: "%N" %}
<textarea id="code{{ nanosecond }}" style="visibility:hidden">{{ code }}</textarea>
<button id="btn" onclick="copyCode( 'code{{ nanosecond }}' )"
data-clipboard-target="#code{{ nanosecond }}">Copy</button>

<script>
function copyCode( queryId ) {
  console.log(queryId)
  var copyText = document.getElementById( queryId );
  console.log(copyText)
  navigator.clipboard.writeText(copyText.textContent);
  }
</script>
