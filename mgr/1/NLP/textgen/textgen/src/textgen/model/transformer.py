from typing import Optional, Tuple, Dict, Callable

import torch
import torchmetrics
from pytorch_lightning import LightningModule
from textgen.data.utils import SentenceCompletionConfig
from textgen.model.layers import PositionalEncoder, EncoderLayer, LayerNormalization, DecoderLayer
from textgen.model.pickers import IndexPicker
from torch import nn, Tensor, tensor


class TransformerModel(nn.Module):
    def __init__(self, src_vocab_size: int, trg_vocab_size: int,
                 d_model: int, d_ff: int, num_heads: int, num_layers: int,
                 drop_out_rate: float, max_seq_len: int,
                 src_embedding: Optional[Tensor] = None, trg_embedding: Optional[Tensor] = None) -> None:
        super().__init__()

        if src_embedding is not None and trg_embedding is not None:
            self.src_embedding = nn.Embedding(src_vocab_size, d_model).from_pretrained(src_embedding, freeze=True)
            self.trg_embedding = nn.Embedding(trg_vocab_size, d_model).from_pretrained(trg_embedding, freeze=True)
        else:
            self.src_embedding = nn.Embedding(src_vocab_size, d_model)
            self.trg_embedding = nn.Embedding(trg_vocab_size, d_model)

        self.drop_out_rate = drop_out_rate
        self.drop_out_enc_in = nn.Dropout(self.drop_out_rate)
        self.drop_out_dec_in = nn.Dropout(self.drop_out_rate)

        self.positional_encoder = PositionalEncoder(d_model, max_seq_len)
        self.encoder = Encoder(d_model, d_ff, num_heads, num_layers, d_model // num_heads, drop_out_rate)
        self.decoder = Decoder(d_model, d_ff, num_heads, num_layers, d_model // num_heads, drop_out_rate)
        self.output_linear = nn.Linear(d_model, trg_vocab_size)
        self.softmax = nn.LogSoftmax(dim=-1)

    def forward(self, src_input: Tensor, trg_input: Tensor,
                e_mask: Optional[Tensor] = None, d_mask: Optional[Tensor] = None) -> Tensor:
        src_input = self.src_embedding(src_input)  # (B, L) => (B, L, d_model)
        src_input = self.drop_out_enc_in(src_input)
        trg_input = self.trg_embedding(trg_input)  # (B, L) => (B, L, d_model)
        trg_input = self.drop_out_dec_in(trg_input)
        src_input = self.positional_encoder(src_input)  # (B, L, d_model) => (B, L, d_model)
        trg_input = self.positional_encoder(trg_input)  # (B, L, d_model) => (B, L, d_model)

        if e_mask is not None:
            e_mask = e_mask.unsqueeze(1)  # (B, L) => (B, 1, L)
        if d_mask is not None:
            d_mask = d_mask.unsqueeze(1)  # (B, L) => (B, 1, L)
            nopeak_mask = torch.ones([1, d_mask.shape[-1], d_mask.shape[-1]],
                                     dtype=torch.bool, device=d_mask.device)  # (1, L, L)
            nopeak_mask = torch.tril(nopeak_mask)  # (1, L, L) to triangular shape (look-ahead mask)
            d_mask = d_mask & nopeak_mask  # (B, L, L) padding false

        e_output = self.encoder(src_input, e_mask)  # (B, L, d_model)

        d_output = self.decoder(trg_input, e_output, e_mask, d_mask)  # (B, L, d_model)
        return self.softmax(self.output_linear(d_output))  # (B, L, d_model) => # (B, L, trg_vocab_size)


class Encoder(nn.Module):
    def __init__(self, d_model: int, d_ff: int, num_heads: int,
                 num_layers: int, d_k: int, drop_out_rate: float) -> None:
        super().__init__()
        self.num_layers = num_layers
        self.layers = nn.ModuleList(
            [EncoderLayer(d_model, d_ff, num_heads, d_k, drop_out_rate) for _ in range(num_layers)]
        )
        self.layer_norm = LayerNormalization(d_model)

    def forward(self, x: Tensor, e_mask: Optional[Tensor] = None) -> Tensor:
        for i in range(self.num_layers):
            x = self.layers[i](x, e_mask)

        return self.layer_norm(x)


class Decoder(nn.Module):
    def __init__(self, d_model: int, d_ff: int, num_heads: int,
                 num_layers: int, d_k: int, drop_out_rate: float) -> None:
        super().__init__()
        self.num_layers = num_layers
        self.layers = nn.ModuleList(
            [DecoderLayer(d_model, d_ff, num_heads, d_k, drop_out_rate) for _ in range(num_layers)]
        )
        self.layer_norm = LayerNormalization(d_model)

    def forward(self, x: Tensor, e_output: Tensor,
                e_mask: Optional[Tensor] = None, d_mask: Optional[Tensor] = None) -> Tensor:
        for i in range(self.num_layers):
            x = self.layers[i](x, e_output, e_mask, d_mask)

        return self.layer_norm(x)


class Transformer(LightningModule):
    """LightningModule for Transformer."""

    def __init__(self, model: TransformerModel, config: SentenceCompletionConfig, index_picker: IndexPicker,
                 loss: Optional[Callable[[Tensor, Tensor], Tensor]] = None,
                 metric: Optional[Callable[[Tensor, Tensor], Tensor]] = None,
                 lr: float = 1e-3, betas: Tuple[float, float] = (0.9, 0.98),
                 teacher_forcing_val: bool = False) -> None:
        super().__init__()
        self.model = model
        self.config = config
        self.index_picker = index_picker
        self.loss = loss or nn.NLLLoss(ignore_index=self.config.corpus.pad_id)
        self.metric = metric or torchmetrics.Accuracy(ignore_index=self.config.corpus.pad_id)
        self.lr = lr
        self.betas = betas
        self.teacher_forcing_val = teacher_forcing_val
        self.save_hyperparameters()

    def forward(self, src: Tensor, trg: Tensor,
                src_mask: Optional[Tensor] = None, trg_mask: Optional[Tensor] = None) -> Tensor:
        return self.model(src, trg, src_mask, trg_mask)

    def generate(self, src: Tensor) -> Tuple[Tensor, Tensor]:
        out = [self._generate_sentence(batch) for batch in src]
        return torch.stack([x[0] for x in out]), torch.stack([x[1] for x in out])

    def _generate_sentence(self, src: Tensor) -> Tuple[Tensor, Tensor]:
        src = src.unsqueeze(0)
        src_mask = src != self.config.corpus.pad_id
        trg = tensor([self.config.padder.pad_with_index([])], device=self.device)
        preds, indices = [], []
        chosen = self.config.corpus.start_id
        for i in range(self.config.max_length):
            trg[0][i] = chosen
            out = self(src, trg, src_mask, trg != self.config.corpus.pad_id)
            pred = out[0][i]
            chosen = self.index_picker.pick_index(pred)
            preds.append(pred)
            indices.append(chosen)
            if chosen == self.config.corpus.end_id:
                preds.extend([pred] * (self.config.max_length - 1 - i))
                indices.extend([tensor(self.config.corpus.pad_id,
                                       device=self.device)] * (self.config.max_length - 1 - i))
                break
        return torch.stack(preds), torch.stack(indices)

    def training_step(self,
                      batch: Tuple[Tensor, Tensor, Optional[Tensor], Optional[Tensor], Tensor],
                      batch_idx: int) -> Tensor:
        src, trg, src_mask, trg_mask, y = batch
        pred = self.model(src, trg, src_mask, trg_mask).transpose(1, 2)  # (B, L, C) => (B, C, L)
        loss = self.loss(pred, y)
        metric = self.metric(pred.exp(), y)  # accuracy needs normal probabilities
        metrics = {'train_metric': metric, 'train_loss': loss}
        self.log_dict(metrics)
        return loss

    def validation_step(self,
                        batch: Tuple[Tensor, Tensor, Optional[Tensor], Optional[Tensor], Tensor],
                        batch_idx: int) -> Dict[str, Tensor]:
        src, trg, src_mask, trg_mask, y = batch
        pred = self(src, trg, src_mask, trg_mask) if self.teacher_forcing_val else self.generate(src)[0]
        pred = pred.transpose(1, 2)  # (B, L, C) => (B, C, L)
        loss = self.loss(pred, y)
        metric = self.metric(pred.exp(), y)  # accuracy needs normal probabilities
        metrics = {'val_metric': metric, 'val_loss': loss}
        self.log_dict(metrics)
        return metrics

    def test_step(self,
                  batch: Tuple[Tensor, Tensor, Optional[Tensor], Optional[Tensor], Tensor],
                  batch_idx: int) -> Dict[str, Tensor]:
        src, trg, src_mask, trg_mask, y = batch
        pred = self(src, trg, src_mask, trg_mask) if self.teacher_forcing_val else self.generate(src)[0]
        pred = pred.transpose(1, 2)  # (B, L, C) => (B, C, L)
        loss = self.loss(pred, y)
        metric = self.metric(pred.exp(), y)  # accuracy needs normal probabilities
        metrics = {'test_metric': metric, 'test_loss': loss}
        self.log_dict(metrics)
        return metrics

    def configure_optimizers(self):
        optimizer = torch.optim.Adam(self.parameters(), lr=self.lr, betas=self.betas)
        return optimizer
