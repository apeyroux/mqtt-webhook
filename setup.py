from setuptools import setup, find_packages

with open('requirements.txt') as f:
    requirements = f.read().splitlines()

setup(
    name='mqtt-webhook',
    version_format='v1.0',
    description='WebHook MQTT',
    packages=find_packages(),
    install_requires=requirements,
    scripts=['src/mqtt-webhook'],
)
