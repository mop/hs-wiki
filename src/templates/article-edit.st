<h1>Edit Article $articleName$</h1>
$if(errorMessages)$
<span class="error">$errorMessages$</span>
$endif$

</script>
<form action="/articles/$articleName$/edit" method="post">
    <p>
        <label for="name">Name: (*)</label>
        <input type="text" name="name" id="name" value="$articleName$" />
    </p>
    <p>
    	<label for="categories">Categories: </label>
      <span class="description">e.g.: Category1, Category2</span>
      <input type="text" id="categories" name="categories" 
       value="$articleCategories$" />
    </p>
    <p>
        <label for="content">Content: (*)</label>
        <textarea class="wymeditor" name="content" id="content">$articleContent$</textarea>
    </p>
    <p>
        <input type="submit" value="Update Article" class="wymupdate" />
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
