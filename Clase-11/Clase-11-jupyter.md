# Parser de dependencias

## Requerimientos
- spaCy

## Documentación
- https://spacy.io/usage/linguistic-features

## Demo parser de dependencias
- https://explosion.ai/demos/displacy


```python
import spacy
```


```python
# Para ver los idiomas y modelos disponibles por idioma: https://spacy.io/usage/models
# Se puede entrar a cada modelo y ver en qué tareas y corpus fue entrenado

#!python -m spacy download en_core_web_md
#!python -m spacy download es_core_news_sm
#!python -m spacy download es_core_news_md
```


```python
nlp_sm = spacy.load("es_core_news_sm")
nlp_sm.pipeline
```




    [('tok2vec', <spacy.pipeline.tok2vec.Tok2Vec at 0x7f51fdec8160>),
     ('morphologizer',
      <spacy.pipeline.morphologizer.Morphologizer at 0x7f51fdec8340>),
     ('parser', <spacy.pipeline.dep_parser.DependencyParser at 0x7f51fde19eb0>),
     ('attribute_ruler',
      <spacy.pipeline.attributeruler.AttributeRuler at 0x7f51fdd89400>),
     ('lemmatizer',
      <spacy.lang.es.lemmatizer.SpanishLemmatizer at 0x7f51fdd8bc00>),
     ('ner', <spacy.pipeline.ner.EntityRecognizer at 0x7f51fde19d60>)]




```python
doc_sm = nlp_sm("Probemos el modelo.")
doc_sm.vocab.vectors.shape
```




    (0, 0)




```python
spacy.displacy.render(doc_sm, style="dep")
```


<span class="tex2jax_ignore"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:lang="es" id="413e7ee5d2604e1ebff0628acc3586d2-0" class="displacy" width="575" height="312.0" direction="ltr" style="max-width: none; height: 312.0px; color: #000000; background: #ffffff; font-family: Arial; direction: ltr">
<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="50">Probemos</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="50">VERB</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="225">el</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="225">DET</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="400">modelo.</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="400">NOUN</tspan>
</text>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-413e7ee5d2604e1ebff0628acc3586d2-0-0" stroke-width="2px" d="M245,177.0 C245,89.5 395.0,89.5 395.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-413e7ee5d2604e1ebff0628acc3586d2-0-0" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">det</textPath>
    </text>
    <path class="displacy-arrowhead" d="M245,179.0 L237,167.0 253,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-413e7ee5d2604e1ebff0628acc3586d2-0-1" stroke-width="2px" d="M70,177.0 C70,2.0 400.0,2.0 400.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-413e7ee5d2604e1ebff0628acc3586d2-0-1" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">obj</textPath>
    </text>
    <path class="displacy-arrowhead" d="M400.0,179.0 L408.0,167.0 392.0,167.0" fill="currentColor"/>
</g>
</svg></span>



```python
nlp = spacy.load("es_core_news_md")
nlp.pipeline
```




    [('tok2vec', <spacy.pipeline.tok2vec.Tok2Vec at 0x7f51fdd4ffa0>),
     ('morphologizer',
      <spacy.pipeline.morphologizer.Morphologizer at 0x7f51fdd4fe20>),
     ('parser', <spacy.pipeline.dep_parser.DependencyParser at 0x7f51fdb52900>),
     ('attribute_ruler',
      <spacy.pipeline.attributeruler.AttributeRuler at 0x7f51fda444c0>),
     ('lemmatizer',
      <spacy.lang.es.lemmatizer.SpanishLemmatizer at 0x7f51fda4e1c0>),
     ('ner', <spacy.pipeline.ner.EntityRecognizer at 0x7f51fdb52890>)]




```python
doc = nlp("Probemos el modelo.")
doc.vocab.vectors.shape
```




    (20000, 300)




```python
spacy.displacy.render(doc, style="dep")
```


<span class="tex2jax_ignore"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:lang="es" id="6648883063dd41d987d0399b1b9b27a9-0" class="displacy" width="575" height="312.0" direction="ltr" style="max-width: none; height: 312.0px; color: #000000; background: #ffffff; font-family: Arial; direction: ltr">
<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="50">Probemos</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="50">VERB</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="225">el</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="225">DET</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="400">modelo.</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="400">NOUN</tspan>
</text>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-6648883063dd41d987d0399b1b9b27a9-0-0" stroke-width="2px" d="M245,177.0 C245,89.5 395.0,89.5 395.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-6648883063dd41d987d0399b1b9b27a9-0-0" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">det</textPath>
    </text>
    <path class="displacy-arrowhead" d="M245,179.0 L237,167.0 253,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-6648883063dd41d987d0399b1b9b27a9-0-1" stroke-width="2px" d="M70,177.0 C70,2.0 400.0,2.0 400.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-6648883063dd41d987d0399b1b9b27a9-0-1" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">obj</textPath>
    </text>
    <path class="displacy-arrowhead" d="M400.0,179.0 L408.0,167.0 392.0,167.0" fill="currentColor"/>
</g>
</svg></span>



```python
for token in doc:
    print(token.text, token.vector)
    print("-----------------------")
```

    Probemos [ 0.29921   1.6466    0.37279  -0.041139 -0.61525  -1.3985   -0.3574
      2.7041    0.39475   1.7172   -0.09514   1.568    -2.3063    1.7408
     -0.4542    2.7913    1.6895    3.0809    0.96411  -1.4807    0.56382
     -0.59703   0.061377  0.27553  -1.4587   -0.88662   1.0452   -1.5169
      1.5448    1.1546   -0.077919  3.2424   -0.54825  -0.88571   0.025792
     -1.3085    1.9598   -1.9706    0.15112  -0.13259   0.94146  -0.050264
      0.81043  -0.78148  -1.1998   -1.0038   -1.0448   -2.6653    0.54582
      2.8315    4.5676    0.58226   1.5525    2.6089    0.35201   1.6957
      2.3347   -0.24905   1.5727   -1.355    -2.221    -1.2386    0.49713
     -0.92971  -1.9165   -0.32358  -0.50634  -0.12671  -0.88943   3.1373
      1.4364    2.9651    1.2749    1.7228    1.2847    0.099094 -0.31655
     -0.049168  0.97356   0.59483  -0.68999   0.93531   0.38891   0.43062
     -0.7087    2.6456    1.8739    1.0637   -2.5568   -1.8739   -1.5072
      3.1777   -3.3031    1.6034   -1.1202   -0.50816   2.603     1.3823
      1.6802    3.2218   -1.2681   -0.57496  -0.043476 -0.29725  -0.36401
      1.8342   -0.32392   0.19348  -2.3577    0.5663   -0.695    -0.41229
     -0.18734  -0.15856   4.9075   -3.0217    1.7902    0.98913  -3.7456
     -0.15828  -1.7188    1.7035    0.15464  -2.8568    3.1187   -1.242
      2.5502    0.4285    0.88945   0.075465  1.9571    2.2542    1.0739
      2.8809    1.8468    0.48445  -1.0313    3.0478   -0.65285   0.99626
     -1.3799   -0.24232  -0.67655   0.051034 -0.47727   1.7585    0.12679
      0.21876  -0.87667  -1.0496   -2.5768   -2.1968    0.034388  2.7438
      0.23688   1.7262   -1.8423   -1.5577    1.0572    1.7354    1.1821
     -1.3162    0.97362   2.4619    2.609    -1.3663   -0.082811 -2.045
     -0.5179    1.6136    0.46877   1.7057    0.61977  -3.7828    0.47151
      0.46044  -1.0865    0.46013   0.13594   1.2039   -0.5124    3.6014
     -0.75821  -0.53818  -1.3652   -0.43506  -1.6382    0.41691   0.69431
     -0.67933   4.4024   -0.76829  -1.8411   -1.4999   -2.348    -2.2458
      1.2174    0.27902  -1.1669   -1.249     0.87239   1.3574    0.90274
     -1.1418   -0.96365   1.9003   -0.068808  1.0133    2.5567   -1.4309
     -0.87237  -0.045765  1.2462   -1.0882   -0.10938  -0.94998   2.7722
     -0.55084  -1.5744    0.94742  -0.17684   0.40664   1.2986    2.987
      1.2301   -1.3532    2.6457    0.75875   1.2458   -1.0905    2.479
     -1.3884   -2.4958    2.3701   -2.5643   -0.72881   0.94809  -0.05313
      2.9068   -0.20201  -1.0579   -0.70823  -0.50287  -0.26926   0.79138
     -0.29764   0.90417   0.95774   0.45072   1.2825    1.2707   -0.090902
      0.49446   1.8182   -0.56646   1.0908    1.266    -1.7853    3.1496
      1.4041   -1.2237    0.57552   0.017553 -1.7707    1.021    -2.182
     -1.5966    2.3024   -1.4924   -0.3875    1.8695    2.3207    1.1407
     -0.96498   1.6305    1.1517    1.9672    2.2719   -0.077659 -0.15393
     -0.014638  0.21146  -1.0332    0.38585   0.53102   0.60949   1.6347
      0.74524   2.7622    2.3867   -1.3547    2.267    -1.1992   -1.0274
     -0.98725  -3.7355   -0.93753   1.0029    0.97846  -1.8275  ]
    -----------------------
    el [-1.6194e+00  1.1347e+01 -1.8904e+00  4.8450e+00 -1.2819e+00  1.2499e+00
      1.1410e+00 -1.0732e+00 -3.9207e+00 -3.0300e+00  2.7747e+00 -2.6446e+00
      1.4155e+00 -4.2990e+00 -4.4629e+00  3.9047e+00  6.6678e+00  5.1291e+00
     -9.1953e-01  5.8249e+00 -6.3079e+00 -2.8351e+00 -6.1279e+00  1.7411e+00
      5.6919e-01 -1.2752e+00  1.0035e+01 -1.6350e+00 -5.5863e-01  1.5150e+00
     -4.3747e+00 -3.6658e+00 -6.0558e+00  2.3227e+00  2.5476e+00 -4.9973e+00
     -1.7275e+00  5.2920e+00  3.4221e+00 -4.0181e+00 -2.0203e+00  1.6255e+00
     -2.3406e+00 -6.4514e-01  2.8294e+00 -2.0511e+00  2.6727e+00 -3.5793e+00
     -1.1397e+00  1.1451e+00 -1.5736e-01 -9.8520e-01  4.1442e+00  2.2519e-01
      1.0002e+01  9.6069e-01 -2.1536e+00 -7.7299e-01  2.1900e-01  3.9840e+00
     -7.6648e+00  1.6327e+00 -4.1159e-01 -3.3739e+00 -1.6712e+00 -1.5445e+00
     -4.1791e+00 -2.8420e+00 -4.2715e+00 -1.0546e+01 -2.2345e+00  3.0528e+00
     -3.6235e+00  5.0227e-01  1.4543e+00  6.9908e+00  3.1303e+00  2.9104e+00
     -1.8322e+00  4.0904e-01 -6.6042e+00 -1.3130e+00 -5.1803e+00  2.0358e+00
      3.4134e+00 -1.5555e+00  4.8932e+00 -2.5680e+00  4.3675e+00  5.5869e+00
      1.4950e+00  1.0575e+00 -3.5765e-01  1.1400e+00 -4.2757e+00  3.3750e-01
     -6.9376e+00 -6.4451e+00  8.0452e-01 -7.3848e+00  5.7876e+00 -7.3387e-01
     -3.4360e-01  3.7244e+00 -1.1423e+00  9.7336e-01  2.1388e+00 -2.8150e+00
     -1.5643e+00  5.4367e+00  2.0391e+00  3.8064e+00 -3.9247e+00 -3.3331e-01
     -5.9569e+00 -2.3028e+00  6.6694e+00  2.4108e+00  2.0240e+00 -3.4942e+00
      4.3539e+00  5.1150e+00 -2.3659e+00 -5.1908e+00 -9.6141e-01  5.3662e+00
      4.3520e+00  1.0616e+00 -1.2914e+00  2.7530e-01  3.3759e+00 -6.6851e+00
     -7.5348e+00 -4.1733e+00  3.4574e+00 -2.3301e+00  3.1126e+00 -2.8756e+00
      5.6057e+00  3.8775e+00  5.7499e+00 -1.7386e+00 -6.2279e+00  2.8040e+00
      1.4850e+00 -1.4534e+00 -5.2499e+00 -2.1933e+00 -9.0344e+00  3.8645e+00
     -2.7033e+00  1.9433e+00  4.8164e+00  2.0240e+00  4.1124e+00  1.6993e+00
     -1.9861e+00  6.7220e+00 -2.9288e+00 -9.1749e-01  1.0529e+00  2.2119e+00
      3.0505e-02  1.0096e+00 -7.4927e-01 -4.7406e+00 -2.5343e+00  4.1691e+00
      3.2231e+00 -2.1702e+00 -1.7250e+00 -1.6214e+00  4.2200e-01 -1.1267e+00
      6.6108e+00 -1.1169e+00 -2.2596e+00 -6.6944e+00  1.3857e+00  4.0944e-01
      3.2637e+00 -4.3699e+00 -1.3797e+00 -4.3116e+00  4.7802e+00  1.2648e+00
      2.3711e+00  3.6250e+00 -5.5053e+00 -3.8540e+00 -1.3583e+00 -8.1370e+00
     -2.7850e+00 -4.1920e-01  1.1294e+01 -1.3872e+00 -5.2222e+00  5.9752e+00
     -2.6804e+00 -4.8475e-01 -1.2667e+00 -4.6246e+00 -1.8745e+00 -8.7154e-03
     -7.5343e+00  3.0278e+00  2.7208e+00  3.9271e+00  7.8610e+00 -6.0735e-01
     -5.9950e+00 -3.2069e+00  3.2893e+00 -3.6901e+00 -1.2611e+00  6.4825e+00
      1.8334e+00  7.3638e-01 -7.4772e+00 -1.7596e+00 -4.0096e+00  2.5252e+00
      2.0351e+00 -6.0539e+00  5.5407e+00  1.4168e+00 -4.7022e+00  7.9057e-01
     -3.1030e-01  8.3223e+00  4.0636e+00  8.4533e+00  3.0833e+00  6.7511e+00
      9.5105e-01 -2.4921e+00  9.6078e-01  2.8673e-01  1.3869e+00  2.5674e+00
     -4.1305e+00 -1.2146e+00 -8.3519e+00  1.4055e+00 -7.1359e+00  4.4286e+00
     -9.2349e-01  1.9586e+00  3.9625e+00  2.3751e+00  4.7878e+00  3.5750e+00
      2.1714e+00  6.6257e+00  1.2945e+01 -7.4724e+00 -2.0055e+00 -2.9313e+00
     -1.1392e+00  6.0554e-01  2.5712e+00  3.7972e+00 -2.0004e+00  4.3857e+00
      2.5096e+00 -5.4046e+00 -7.8543e+00 -5.8777e+00  4.3577e+00  2.0738e+00
     -5.1361e+00 -3.4258e+00  5.9812e+00 -7.2061e-01  1.6959e+00 -8.5744e+00
      1.4188e+00  6.9821e+00  5.5734e+00 -4.1414e+00 -1.1906e+00 -2.4569e+00
     -1.0163e-01  9.5856e-01  4.8782e+00  5.2247e+00 -8.0482e+00 -9.8634e+00
      6.1902e+00  1.0616e+00  4.6732e+00 -2.9907e+00 -3.1376e+00 -3.6174e+00
     -1.3083e+00 -2.4310e+00  4.6911e+00  4.1889e+00  1.7969e+00  4.7222e-01]
    -----------------------
    modelo [-5.2584e-01 -8.3320e-01 -2.8002e+00 -1.4607e-01 -1.3450e+00 -3.8575e-01
     -5.1929e-01 -1.2352e-01  1.8054e-01 -1.7019e+00 -2.1074e-01  1.0664e+00
      1.4504e+00  1.6647e+00 -4.0520e-01 -1.2232e+00  3.1858e-01  3.8393e-01
      1.6948e+00 -7.7783e-01 -8.0421e-01  6.5311e-01 -2.0100e-01 -1.1493e+00
     -6.9275e-01 -1.0540e-03 -8.2782e-01  4.3541e-01  2.8740e+00 -1.2434e+00
      3.1570e+00 -4.3222e-01  9.5334e-01  2.6557e+00 -1.7379e+00 -7.8826e-01
      1.4696e+00 -3.9317e-01  2.7724e+00 -7.3109e-01 -5.0225e-02 -1.0747e-01
     -3.6775e-01  4.4344e-01 -9.8950e-01 -1.9438e+00 -2.1840e+00  2.9962e+00
     -1.0697e+00 -3.4491e-01 -3.5008e-02  1.3473e+00 -3.5020e-01  3.4729e+00
      7.6487e-01 -3.3631e-01  1.2327e-01  5.4021e-01 -2.8924e+00  1.8864e+00
     -1.0109e-02  1.3393e+00  1.0833e+00  3.2705e-01  2.5216e+00  1.0930e+00
     -7.6518e-01 -2.3739e-01 -3.8141e-01  3.3036e-01 -1.7435e+00  2.3308e-01
      1.5260e+00 -3.6727e-01 -3.1365e+00  2.7433e+00  6.0120e-01  5.5424e-01
     -3.6695e+00 -7.3615e-01 -1.9999e+00  1.4826e+00 -6.5424e-02  1.2398e+00
      2.3646e+00  1.7457e+00  7.7600e-02 -1.5807e+00  9.6478e-01  2.9299e-01
      7.5233e-01 -3.4301e+00  3.8824e-01  3.4119e+00  2.7811e+00  1.4886e+00
      6.4065e-01 -1.4204e+00 -1.4085e+00 -5.0765e-01  3.0958e-01 -1.5481e+00
      3.5869e+00  1.5506e+00 -2.7005e-01  1.7295e+00 -7.0200e-01 -2.8197e+00
      3.2170e+00 -1.4404e+00 -9.2789e-01 -1.3722e+00 -7.0937e-01  2.9913e+00
      4.9902e-01 -5.6546e-01  2.2157e+00  1.3268e+00 -4.5619e+00  1.1540e+00
     -6.1560e-01  1.6493e+00 -9.6853e-01 -1.0163e+00  2.8416e+00  1.1176e+00
     -2.2570e-01  1.5182e+00  2.0498e+00 -9.6557e-01 -2.0108e+00  5.9454e-01
      3.7695e+00 -6.1929e-01 -1.0267e+00 -3.7689e-01 -7.9091e-01  2.0392e-01
     -1.3255e-01 -7.7563e-01 -6.2684e-01  4.1907e+00  1.0689e+00 -2.8830e+00
      2.4225e+00  3.3718e+00  2.5274e+00 -2.7072e+00 -1.8189e+00 -3.9238e-01
     -1.9558e+00 -9.8115e-03  1.9617e+00  1.2412e+00  2.8971e-01 -6.4207e-01
      1.7618e+00  5.1947e-01  1.8813e-01  4.5011e-01  3.6289e+00  1.3270e+00
      1.3540e+00 -3.7112e-01 -2.7756e-01 -2.3126e+00 -1.3114e+00 -1.1745e+00
      2.0672e+00 -1.0405e-01  1.1261e+00  5.1842e-01 -2.1540e-01 -4.3587e+00
      1.0691e+00 -3.9896e-01 -1.5831e+00 -3.5860e+00 -1.9101e+00 -2.1686e+00
     -1.1393e-01  1.4760e-01  6.3548e-01  7.9353e-01  6.9923e-01 -3.4031e-01
     -1.7379e+00  5.0603e-01 -1.0938e+00 -5.8746e-01  4.4533e-01 -3.2301e+00
      1.8913e+00  3.6723e+00 -1.3207e+00  4.7473e-01  3.6336e+00  1.4617e+00
      3.9115e+00 -6.6592e-01  4.1616e-02  6.3603e-02 -1.3598e+00 -2.5666e+00
      2.1875e+00  3.8293e-01 -1.5875e+00  3.9665e+00 -6.0052e-01  1.6435e+00
      2.4941e+00 -6.8453e-01 -4.4270e+00 -1.6501e+00 -2.0219e+00 -6.5137e-01
     -6.2415e-01  3.9203e-01 -8.6940e-01 -4.1346e+00 -8.9258e-01 -1.6843e+00
      1.2211e+00 -1.2568e-01  7.9006e-01 -3.1785e+00  3.4739e-02  8.4185e-01
     -6.2321e-01  2.8213e+00 -6.3492e-01 -1.2019e+00 -8.8578e-01  1.7979e+00
     -1.6701e+00  1.1274e+00  4.0966e+00 -3.3981e-01  5.2912e-01  8.8382e-01
      8.1059e-01  1.0343e+00 -3.7677e+00 -1.5976e+00 -2.1669e+00 -8.7240e-01
     -3.0787e+00 -1.1068e+00 -2.4386e+00 -3.9128e-01  5.8779e-01  1.5739e+00
      1.6102e+00 -1.6038e+00  1.8504e+00  1.0862e+00 -1.8100e-01 -9.5880e-01
      3.8300e+00 -2.1273e+00  1.5570e+00 -7.2300e-01  2.7873e+00  3.5940e-01
      3.6875e+00  8.2844e-01  1.8957e+00  2.0969e+00  4.1655e+00  1.1436e-01
     -1.0301e+00  3.9221e+00  4.5291e-01 -3.1167e-01 -6.6016e-01 -2.7548e+00
     -1.5490e+00 -1.4190e+00 -1.1102e+00 -3.1333e+00 -9.1573e-02  1.2255e+00
      2.1597e-03  2.7183e+00  1.3073e+00 -1.5358e+00  2.6187e+00  9.8344e-01
      3.7171e-01  1.8342e+00  7.4953e-01 -1.3994e+00 -2.1924e+00  9.9185e-01
     -1.5325e+00 -3.8708e+00  4.6127e-01 -1.8929e-01  1.4907e+00 -1.3783e+00]
    -----------------------
    . [ 1.6517   -1.9634   -0.60317  -1.4497   -4.6456   -2.6548    1.7871
      2.0086    2.5595   -6.62     -3.1862    1.4241    1.2941    2.3816
     -2.2026    3.1901   -0.069615  3.4981    0.52599   2.1718    0.68467
     -0.89486  -2.184     1.1761   -0.067467  0.72529   2.7139    0.59838
      2.4567   -8.4852   -1.5334    1.765     0.074411  2.7647   -0.011132
     -3.8116   -2.1176    1.4034   -3.6477    1.435     1.5079   -3.9402
     -2.6653   -2.438    -2.5748    0.78063  -4.2288   -5.5485    1.2281
      3.5425    2.5387   -1.5244   -2.9497    5.9991   -0.013804  0.53404
     -1.8439    0.8998   -3.0015   -1.8224   -2.5184    1.1081   -2.1192
     -1.2046    1.045    -0.61409   1.0887   -2.6562   -0.41009  -2.9739
      3.899     2.4347    0.74896   1.858     1.002     1.6324   -0.90548
      6.6497    0.24717  -0.42803  -0.10038  -2.6809   -1.1805   -3.3625
      2.1539   -0.10481   3.3057    3.112     5.7779   -5.0753   -2.127
      2.1877   -6.2819   -1.6592   -4.0378    2.2019   -1.9772   -1.5248
     -3.5156   -3.1314    0.23034   0.89325   0.33181   1.6749    0.81533
     -0.88029   1.2501   -1.1932    2.5691    0.74317  -0.88583   1.0391
      7.776    -3.162    -2.6456    4.8706    0.61319  -0.68245   1.7849
      3.6145    1.0658   -2.9448    1.2607    2.7204    2.6704   -0.25994
     -1.2124   -0.13574   0.97574   0.10461  -2.35     -7.8595    0.17082
     -2.864    -0.43391   2.6371    0.86701  -1.6068    2.0415   -1.7224
      1.2635    3.1676   -2.7371    4.5863   -1.063     0.68731  -0.6117
     -0.89254   2.0062    1.002    -0.16514   0.62088  -0.93454   1.5502
      1.0427   -3.2031    2.3137    2.2351   -1.1178   -0.016491 -2.593
     -2.4076    1.5886    3.8658   -3.5407    2.3651   -0.1282   -2.2884
     -4.1526    1.5237   -1.6451    3.958    -2.5176    1.271     5.5891
     -0.073714  1.7153    7.2809    0.67752  -0.11758   1.2539   -2.1887
     -1.5756    2.315    -2.6269   -0.78974   1.0015    0.38805  -0.87827
      2.229     0.4337    3.5694    0.77441   2.4085   -0.81812   2.577
      0.17484  -5.6634    0.70201   1.3582    1.3136    1.7878    2.3947
     -0.053988  0.30324   0.8196    0.54558   4.4698   -0.57433  -1.4294
      3.3957   -1.1672    1.2204   -0.30839   2.2115    3.1325    5.957
      5.6923   -6.7601   -0.2131   -0.73857  -0.73549  -2.4655    2.1199
      3.8113    0.60626  -0.5744   -0.39691  -0.49172   3.1223    2.1085
     -0.35509   0.76224   1.7729   -3.1049   -4.8747   -4.7791    1.6022
     -0.42757  -2.6162    0.51915  -2.0794   -0.24428  -0.1492    2.1878
      4.5774   -2.5017    2.8651   -1.6572    1.9492    1.4551    0.46257
     -1.8519    4.2251    2.1565    3.3613   -1.3283    2.9527    1.9768
      2.5539    1.4193    6.8043    2.6752   -2.2353   -0.17639   1.1861
      0.9132    1.835     0.20752   1.6543    0.99623  -1.8868    1.8383
     -1.7074    0.28406   4.3414   -0.044696  1.4921    0.49855   1.0152
      2.1297    0.19676   2.6029    3.414     2.5571    5.8898    1.7887
      3.6939    3.3534   -2.6737   -2.9593    0.7448    2.1898    0.79826
      2.6447   -1.9847   -3.3541   -0.39062  -1.8326   -3.0347  ]
    -----------------------



```python
doc1 = nlp("Probemos el modelo")
doc2 = nlp("Ah la flauta")
```


```python
print(doc1, "<->", doc2, doc1.similarity(doc2))
```

    Probemos el modelo <-> Ah la flauta -0.09253401798253823



```python
doc1_vec = doc1.vector
doc2_vec = doc2.vector
```


```python
type(doc1_vec)
```




    numpy.ndarray




```python
len(doc1_vec)
```




    300




```python
for token in doc:
    print(token.text, token.dep_, token.head.text, token.head.pos_,
            [child for child in token.children])
```

    Probemos ROOT Probemos VERB [modelo, .]
    el det modelo NOUN []
    modelo obj Probemos VERB [el]
    . punct Probemos VERB []



```python
doc = nlp("¿quiénes dijo Juan que estornudaron?")
spacy.displacy.render(doc, style="dep")
```


<span class="tex2jax_ignore"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:lang="es" id="ef63d41fac394e27a11e741c240dd9ae-0" class="displacy" width="1100" height="312.0" direction="ltr" style="max-width: none; height: 312.0px; color: #000000; background: #ffffff; font-family: Arial; direction: ltr">
<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="50">¿</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="50">PUNCT</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="225">quiénes</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="225">PRON</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="400">dijo</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="400">VERB</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="575">Juan</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="575">PROPN</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="750">que</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="750">SCONJ</tspan>
</text>

<text class="displacy-token" fill="currentColor" text-anchor="middle" y="222.0">
    <tspan class="displacy-word" fill="currentColor" x="925">estornudaron?</tspan>
    <tspan class="displacy-tag" dy="2em" fill="currentColor" x="925">VERB</tspan>
</text>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-ef63d41fac394e27a11e741c240dd9ae-0-0" stroke-width="2px" d="M70,177.0 C70,2.0 400.0,2.0 400.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-ef63d41fac394e27a11e741c240dd9ae-0-0" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">punct</textPath>
    </text>
    <path class="displacy-arrowhead" d="M70,179.0 L62,167.0 78,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-ef63d41fac394e27a11e741c240dd9ae-0-1" stroke-width="2px" d="M245,177.0 C245,89.5 395.0,89.5 395.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-ef63d41fac394e27a11e741c240dd9ae-0-1" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">obj</textPath>
    </text>
    <path class="displacy-arrowhead" d="M245,179.0 L237,167.0 253,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-ef63d41fac394e27a11e741c240dd9ae-0-2" stroke-width="2px" d="M420,177.0 C420,89.5 570.0,89.5 570.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-ef63d41fac394e27a11e741c240dd9ae-0-2" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">nsubj</textPath>
    </text>
    <path class="displacy-arrowhead" d="M570.0,179.0 L578.0,167.0 562.0,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-ef63d41fac394e27a11e741c240dd9ae-0-3" stroke-width="2px" d="M770,177.0 C770,89.5 920.0,89.5 920.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-ef63d41fac394e27a11e741c240dd9ae-0-3" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">mark</textPath>
    </text>
    <path class="displacy-arrowhead" d="M770,179.0 L762,167.0 778,167.0" fill="currentColor"/>
</g>

<g class="displacy-arrow">
    <path class="displacy-arc" id="arrow-ef63d41fac394e27a11e741c240dd9ae-0-4" stroke-width="2px" d="M595,177.0 C595,2.0 925.0,2.0 925.0,177.0" fill="none" stroke="currentColor"/>
    <text dy="1.25em" style="font-size: 0.8em; letter-spacing: 1px">
        <textPath xlink:href="#arrow-ef63d41fac394e27a11e741c240dd9ae-0-4" class="displacy-label" startOffset="50%" side="left" fill="currentColor" text-anchor="middle">flat</textPath>
    </text>
    <path class="displacy-arrowhead" d="M925.0,179.0 L933.0,167.0 917.0,167.0" fill="currentColor"/>
</g>
</svg></span>



```python
for token in doc:
    print(token.text, token.) 
```


      Input In [17]
        print(token.text, token.)
                                ^
    SyntaxError: invalid syntax




```python
spacy.pipeline.dep_parser.DependencyParser?
```

{% include additional_content.html %}

{% include copybutton.html %}


