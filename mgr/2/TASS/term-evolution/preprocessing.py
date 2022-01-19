import os
from typing import Iterable, List, Tuple

import spacy
from spacy import Language
from spacy.tokens import Token

SPACY_EXCLUDED_PIPELINES = ['parser', 'ner', 'entity_linker', 'entity_ruler',
                            'textcat', 'textcat_multilabel', 'senter',
                            'sentencizer', 'tok2vec', 'transformer']


def load_spacy(model: str) -> Language:
    if not spacy.util.is_package(model):
        spacy.cli.download(model, False, False, ["--quiet"])
    return spacy.load(
        model,
        exclude=SPACY_EXCLUDED_PIPELINES
    )


def is_irrelevant(token: Token) -> bool:
    return token.is_stop or \
           token.is_space or \
           token.is_punct or \
           token.is_quote or \
           token.is_bracket or \
           token.is_digit or \
           token.is_currency or \
           token.like_num or \
           token.like_url or \
           token.like_email


def tokenize(
        texts: Iterable[str],
        model: str = 'en_core_web_sm'
) -> Tuple[List[List[str]], List[str]]:
    nlp = load_spacy(model)
    nlp.max_length = max(len(text) for text in texts) + 1
    tokens = [
        [token.lemma_ for token in doc if not is_irrelevant(token)]
        for doc in nlp.pipe(texts, n_process=os.cpu_count())
    ]
    return tokens, sorted(set(token for tkns in tokens for token in tkns))
