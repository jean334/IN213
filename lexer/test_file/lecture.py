with open('./test1', 'r') as file:
    content = file.read()
    print(content)
    if content.endswith('EOF'):
        print("Le fichier se termine avec le marqueur EOF.")
    else:
        print("Le fichier ne se termine pas avec le marqueur EOF.")
