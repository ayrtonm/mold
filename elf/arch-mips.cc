// https://github.com/rui314/psabi/blob/main/mips.pdf

#include "mold.h"

namespace mold::elf {

using E = MIPS32LE;

template <>
i64 get_addend(u8 *loc, const ElfRel<E> &rel) {
  switch (rel.r_type) {
  case R_MIPS_32:
  case R_MIPS_26:
    return *(il32 *)loc;
  default:
    return 0;
  }
}

template <>
void write_addend(u8 *loc, i64 val, const ElfRel<E> &rel) {
  switch (rel.r_type) {
  case R_MIPS_NONE:
    break;
  case R_MIPS_32:
  case R_MIPS_26:
    *(ul32 *)loc = val;
    break;
  default:
    unreachable();
  }
}

template <> void write_plt_header(Context<E> &, u8 *) { assert(false); }

template <> void write_plt_entry(Context<E> &, u8 *, Symbol<E> &) {
  assert(false);
}

template <> void write_pltgot_entry(Context<E> &, u8 *, Symbol<E> &) {
  assert(false);
}

template <>
void EhFrameSection<E>::apply_reloc(Context<E> &ctx, const ElfRel<E> &rel,
                                    u64 offset, u64 val) {
  assert(false);
}

template <>
void InputSection<E>::apply_reloc_nonalloc(Context<E> &ctx, u8 *base) {
  assert(false);
  std::span<const ElfRel<E>> rels = get_rels(ctx);

  for (i64 i = 0; i < rels.size(); i++) {
    const ElfRel<E> &rel = rels[i];
    if (rel.r_type == R_NONE || record_undef_error(ctx, rel))
      continue;

    Symbol<E> &sym = *file.symbols[rel.r_sym];
    u8 *loc = base + rel.r_offset;

    SectionFragment<E> *frag;
    i64 frag_addend;
    std::tie(frag, frag_addend) = get_fragment(ctx, rel);

    u64 S = frag ? frag->get_addr(ctx) : sym.get_addr(ctx);
    u64 A = frag ? frag_addend : get_addend(*this, rel);
    u64 P = get_addr() + rel.r_offset;

    switch (rel.r_type) {
    case R_MIPS_32:
      *(U32<E> *)loc = S + A;
      break;
    case R_MIPS_26:
      *(U32<E> *)loc = ((A << 2) | (P & 0xF000'0000) + S) >> 2;
      break;
    case R_MIPS_LO16:
      if ((i == 0) || (rels[i - 1].r_type != R_MIPS_HI16)) {
          // TODO: Handle orphaned paired relocation
          assert(false);
      } else {
        u64 AHI = get_addend(*this, rels[i - 1]);
        u64 AHL = (ul16)A + (AHI << 16);
        *(U16<E> *)loc = AHL + S;
      }
      break;
    case R_MIPS_HI16:
      if ((i == rels.size() - 1) || (rels[i + 1].r_type != R_MIPS_LO16)) {
          // TODO: Handle orphaned paired relocation
          assert(false);
      } else {
          u64 ALO = get_addend(*this, rels[i + 1]);
          u64 AHL = (u16)ALO + (A << 16);
        *(U16<E> *)loc = ((AHL + S) - (ul16)(AHL + S)) >> 16;
      }
      break;
    default:
      Fatal(ctx) << *this
                 << ": invalid relocation for non-allocated sections: " << rel;
      break;
    }
  }
}

template <> void InputSection<E>::apply_reloc_alloc(Context<E> &ctx, u8 *base) {
  assert(false);
  InputSection<E>::apply_reloc_nonalloc(ctx, base);
}

template <> void InputSection<E>::scan_relocations(Context<E> &ctx) {
  assert(false);
  assert(shdr().sh_flags & SHF_ALLOC);

  this->reldyn_offset = file.num_dynrel * sizeof(ElfRel<E>);
  std::span<const ElfRel<E>> rels = get_rels(ctx);

  // Scan relocations
  for (i64 i = 0; i < rels.size(); i++) {
    const ElfRel<E> &rel = rels[i];
    if (rel.r_type == R_NONE || record_undef_error(ctx, rel))
      continue;

    Symbol<E> &sym = *file.symbols[rel.r_sym];

    if (sym.is_ifunc())
      sym.flags |= NEEDS_GOT | NEEDS_PLT;

    switch (rel.r_type) {
    case R_MIPS_32:
      scan_dyn_absrel(ctx, sym, rel);
      break;
    case R_MIPS_26:
      scan_absrel(ctx, sym, rel);
      break;
    case R_MIPS_LO16:
    case R_MIPS_HI16:
      break;
    default:
      Error(ctx) << *this << ": unknown relocation: " << rel;
    }
  }
}

} // namespace mold::elf
