<h1>New Article</h1>
$if(errorMessages)$
<span class="error">$errorMessages$</span>
$endif$
<form action="/articles/new" method="post">
    <p>
        <label for="name">Name: (*)</label>
        <input type="text" id="name" name="name" />
    </p>
    <p>
    	<label for="categories">Categories: </label>
        <span class="description">e.g.: Category1, Category2</span>
        <input type="text" id="categories" name="categories" />
    </p>
    <p>
        <label for="content">Text: (*)</label>
        <textarea class="wymeditor" id="content" name="content"></textarea>
    </p>
    <p>
        <input type="submit" value="Create Article" class="wymupdate" />
    </p>
</form>

<script type="text/javascript" src="/javascript/wymeditor/jquery/jquery.js"></script>
<!-- WYMeditor main JS file, packed version -->
<script type="text/javascript" src="/javascript/wymeditor/wymeditor/jquery.wymeditor.pack.js"></script>

<script type="text/javascript">

jQuery(function() {
    jQuery('.wymeditor').wymeditor();
});
</script>
