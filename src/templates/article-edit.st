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
        <label for="content">Content: (*)</label>
        <textarea name="content" id="content">$articleContent$</textarea>
    </p>
    <p>
        <input type="submit" value="Update Article" />
    </p>
</form>
