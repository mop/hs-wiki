<h1>Edit Article $articleName$</h1>
$if(errorMessages)$
<span class="error">$errorMessages$</span>
$endif$
<form action="/articles/$articleName$/edit" method="post">
    <p>
        <label for="name">Name: (*)</label>
        <input type="text" name="name" id="name" value="$articleName$" />
    </p>
    <p>
    	<label for="categories">Cagetories: </label>
	<span class="description">e.g.: Category1, Category2</span>
	<input type="text" id="categories" name="categories" 
	value="$articleCategories$" />
    </p>
    <p>
        <label for="content">Content: (*)</label>
        <textarea name="content" id="content">$articleContent$</textarea>
    </p>
    <p>
        <input type="submit" value="Update Article" />
    </p>
</form>
