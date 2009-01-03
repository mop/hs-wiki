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
    	<label for="categories">Cagetories: </label>
	<input type="text" id="categories" name="categories" />
    </p>
    <p>
        <label for="content">Text: (*)</label>
        <textarea id="content" name="content"></textarea>
    </p>
    <p>
        <input type="submit" value="Create Article" />
    </p>
</form>
