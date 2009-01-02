<h1>Login</h1>
$if(errorMessages)$
<span class="error"> $errorMessages$ </span>
$endif$
<form action="/login" method="post">
    <p>
        <label for="name">Name:</label>
        <span class="error_field">
        $errorName$
        </span>
        <input type="text" name="name" id="name">
    </p>
    <p>
        <label for="password">Password:</label>
        <span class="error_field">
        $errorPassword$
        </span>
        <input type="password" name="password" id="password">
    </p>
    <p>
        <input type="submit" value="Login"/>
    </p>
</form>
