import speech_recognition as sr
from google.cloud import translate_v2 as translate


r = sr.Recognizer()

with sr.Microphone() as source:
    audio = r.listen(source)
try:
    text = r.recognize_google(audio)
except sr.UnknownValueError:
    print("Google Speech Recognition could not understand audio")
except sr.RequestError as e:
    print("Could not request results from Google Speech Recognition service; {0}".format(e))


translate_client = translate.Client()


target = 'es' # target language, change as needed
result = translate_client.translate(text, target_language=target)
print(result['input'], '->', result['translatedText'])
