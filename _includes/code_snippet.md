{% assign code = include.code %}
{% assign language = include.language %}

``` {{ language }}
{{ code }}
```
{% assign nanosecond = "now" | date: "%N" %}
<textarea id="code">{{ code }}</textarea>
<button id="btn" onlclick="copyCode( 'code' )"
data-clipboard-target="#code">Copy</button>

<script>
function copyCode( queryId ) {
  console.log(queryId)
  var copyText = document.getElementById( queryId );
  console.log(copyText)
  navigator.clipboard.writeText(copyText.textContent);
  }
</script>
