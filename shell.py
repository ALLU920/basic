import basic 

# infinite loop that reads from the terminal window
while True:
    text = input('basic >')
    result, error = basic.run('<stdin>', text)

    if error: print(error.as_string())
    else: print(result)