from googletrans import Translator

# Create an instance of the Translator class
translator = Translator()

# Define the text to be translated
text_to_translate = "department of statistics"

# Translate the text to a different language
translated_text = translator.translate(text_to_translate, dest='ko')

# Print the translated text
print(translated_text.text)
