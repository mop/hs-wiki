<h1>Register</h1>
$if(errorMessages)$
<span class="error"> $errorMessages$ </span>
$endif$
<form action="/register" method="post">
    <p>
        <label for="name">(*) Name:</label>
        <span class="error_field">
        $errorName$
        </span>
        <input type="text" name="name" id="name">
    </p>
    <p>
        <label for="mail">(*) Mail:</label>
        <span class="error_field">
        $errorMail$
        </span>
        <input type="text" name="mail" id="mail">
    </p>
    <p>
        <label for="password">(*) Password:</label>
        <span class="error_field">
        $errorPassword$
        </span>
        <input type="password" name="password" id="password">
    </p>
    <p>
        <label for="password_confirmation">(*) Password Confirmation:</label>
        <span class="error_field">
        $errorPasswordConfirmation$
        </span>
        <input type="password" name="password_confirmation"
        id="password_confirmation">
    </p>
    <p>
        <input type="submit" value="Register"/>
    </p>
</form>
