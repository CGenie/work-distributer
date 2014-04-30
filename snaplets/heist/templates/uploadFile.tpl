<html>
    <head>
    </head>
    <body>
        <p>
            <a href="${path}"><path/></a>
        </p>
        <file>
            <form method="POST" action="/uploadFile/${hash}" enctype="multipart/form-data">
                <p>
                    <input name="file" type="file" />
                </p>

                <button type="submit">Submit</button>
            </form>
        </file>
    </body>
</html>
