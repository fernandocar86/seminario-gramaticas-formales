{% assign code = include.code %}
{% assign language = include.language %}

``` {{ language }}
{{ code }}
```
{% assign nanosecond = "now" | date: "%N" %}
<textarea id="code{{ nanosecond }}" style="display:none;">{{ code | xml_escape }}</textarea>
<button id="copybutton{{ nanosecond }}" data-clipboard-target="#code{{ nanosecond }}">
  <img src="assets/clippy.svg.png" alt="Copy to clipboard" />
</button>

<script src="https://unpkg.com/clipboard@2/dist/clipboard.min.js"></script>

<script>
var copybutton = document.getElementById('copybutton{{ nanosecond }}')
var clipboard{{ nanosecond }} = new ClipboardJS(copybutton);

clipboard{{ nanosecond }}.on('success', function(e) {
    console.log(e);
});
clipboard{{ nanosecond }}.on('error', function(e) {
    console.log(e);
});
</script>
