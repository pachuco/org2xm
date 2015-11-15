/* Org2XM v1.0
 * Converts Org songs from Cave Story to XM modules.
 * Implementation 2008-04-02 by Jan "Rrrola" Kadlec. Public Domain.
 *
 * Usage: "org2xm input.org"
 *        "org2xm input.org c" for compatibility (but quality is worse)
 *
 * Credits:
 * - Pixel Studios for making Cave Story and composing all the songs
 * - Pete Mackay for providing details about the original Org player routine
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef signed char    S8;
typedef signed short   S16;
typedef unsigned char  U8;
typedef unsigned short U16;
typedef unsigned int   U32;

#define PACKED __attribute__((packed))

#define read(to, bytes) fread(to, 1, bytes, f)
#define write(from, bytes) fwrite(from, 1, bytes, g)

#define VOL 51  // default volume

//////////////////////////////////////////////////////////////////////// Input
#pragma pack(push,1)
struct OrgHeader
{ U8 magic[6];
  U16 msPerBeat;  // 1..2000
  U8 measuresPerBar;
  U8 beatsPerMeasure;
  U32 loopStart;  // in beats
  U32 loopEnd;
} PACKED header;
#pragma pack(pop)

struct Note
{ U32 start;
  U8 len;
  U8 key;  // 0(C-0)..96(B-7)
  U8 vol;  // 0..255
  U8 pan;  // 0..12
} *note[16];


///////////////////////////////////////////////////// Immediate representation
#pragma pack(push,1)
struct Instrument
{ U16 freqShift;
  U8 sample;  // melody(voice 0..7) 0-99, drums(voice 8..15) 0-11
  U8 noLoop;
  U16 notes;

  U8 drum;
  U8 instrument;
  S8 finetune;

  S8 lastPan;  // encoding state
  U8 lastVol;
  U8 played;
} PACKED t[16];
#pragma pack(pop)

struct Track
{ float freq;  // if (freq != 0), a new note starts
  U8 vol;      // 1..64; if (volume == 0), the note ends
  S8 pan;      // -127=left..127=right
} *n[16];


U8 *pat[256], patTable[256]; int patLen[256]; int patterns;

int instruments;  // number of used instruments
int tracks;       // number of tracks
int barLen;       // pattern length
int rows, bars;   // song length
int loop;         // does the song loop?

U8 buf[65536]; int len;

int compatibility; // reset instrument at each note?


/////////////////////////////////////////////////////////////////////// Output
#pragma pack(push,1)
struct XMHeader
{ U8 id[17];
  U8 moduleName[20];
  U8 eof;
  U8 trackerName[20];
  U16 version;
  U32 headerSize;
  U16 songLength;  // in patterns
  U16 restartPosition;
  U16 channels;  // should be even, but we don't care ;)
  U16 patterns;
  U16 instruments;
  U16 flags;
  U16 tempo;
  U16 bpm;
  U8 patternOrder[256];
} PACKED xmh = {
 "Extended Module: ", "", 0x1A, "Org2XM by Rrrola    ", 0x104, 0x114
};


struct XMInstrument
{ U32 size;
  U8 instrumentName[22];
  U8 zero;
  U16 samples;
  U32 sampleHeaderSize;
  U8 misc[230];
  U32 sampleLength;
  U32 loopStart;
  U32 loopLength;
  U8 volume;
  S8 finetune;
  U8 type;
  U8 panning;
  U8 relativeKey;
  U8 reserved;
  U8 sampleName[22];
} PACKED smp = {
 0x107, "Melody-00", 0, 1, 0x28, {}, 256, 0, 256, VOL, 0, 1, 128, 48, 0, ""
};
#pragma pack(pop)

//////////////////////////////////////////////////////////////////////// Drums

char drumName[][8] = {
 "Bass01", "Bass02", "Snare01", "Snare02", "Tom01", "HiClose",
 "HiOpen", "Crash", "Per01", "Per02", "Bass03", "Tom02"
};


////////////////////////////////////////////////////////// XM pattern encoding
// returns whatever is supposed to be encoded for (track i, row j)
// assumes that (i, j-1) has already been processed

int key, finetune;

U8 wKey, wInst, wVol, wFine, wPan, wPanVol, wSkip;

void encode(int i, int j)
{
  void resetPanVol(void)
  { wPan = (n[i][j].pan != (wInst ? 0 : t[i].lastPan));
    wVol = (n[i][j].vol != (wInst ? VOL : t[i].lastVol));
  }

  if (!j) t[i].lastPan = t[i].lastVol = 0;
  if (j%barLen == 0) t[i].played = 0;  // independent patterns: drop running fx

  wKey = wInst = wVol = wFine = wPan = wPanVol = 0;
  wSkip = (j==header.loopEnd-1 && i==tracks-1);

 // kill looping notes at loop start
  if (j == header.loopStart && !t[i].noLoop) t[i].lastVol = -1;

  if (n[i][j].freq)
  { wKey = 1;

    finetune = (log2(n[i][j].freq/8363)*12 - t[i].finetune/128. + t[i].drum*36)*8 + .5;
    key = (finetune+4) / 8;
    finetune -= key*8;

    wInst = !t[i].played; if (compatibility) wInst = 1;
    wFine = !!finetune;
    resetPanVol();

   // if panning is default, set instrument
    if (wPan && !n[i][j].pan)
    { wInst = 1;
      resetPanVol();
    }

   // if the volume is default and panning can't improve, set instrument
    if (wVol && n[i][j].vol==VOL && (n[i][j].pan!=t[i].lastPan || !n[i][j].pan))
    { wInst = 1;
      resetPanVol();
    }

   // if there's panning with another effect, try panning in volume column
    if (wPan && (wFine||wSkip) && !wVol)
    { wPan = 0;
      wPanVol = 1;
    }
  }
  else
  { if (n[i][j].vol != t[i].lastVol) wVol = 1;
    if (n[i][j].vol && n[i][j].pan != t[i].lastPan) wPan = 1;

   // write note endings
    if (wVol && !n[i][j].vol)
    { wVol = wPan = wFine = 0;
      if (!t[i].noLoop) { wKey = 1; key = 0x60; }
      t[i].lastVol = 0;
    }
  }

  if (wInst) { t[i].lastPan = 0; t[i].lastVol = VOL; t[i].played = 1; }
  if (wVol) { wPanVol = 0; t[i].lastVol = n[i][j].vol; }
  if (wPanVol || wPan) t[i].lastPan = n[i][j].pan;
  if (wSkip) wPan = wFine = 0;
  if (wPan) wFine = 0;
}


///////////////////////////////////////////////////////////////////////// Main


int main(int argc, char** argv)
{ int i, j, k, l, m;
  FILE *f, *g;

  if (argc < 2) goto Err1;

  if (argc > 2) compatibility = 1;

  if (!(f = fopen(argv[1], "rb"))) goto Err2;


 // Read the Org file

  read(&header, sizeof(struct OrgHeader));
  if (memcmp(header.magic, "Org-02", 6)) goto Err3;
  printf("%u\n", sizeof(PACKED struct OrgHeader));
  printf("%u\n", sizeof(U8));
  printf("%u\n", sizeof(U16));
  printf("%u\n", sizeof(U32));

  for (i=0; i<16; ++i)
    read(&t[i], 6);

  for (i=0; i<16; ++i)
  { note[i] = malloc(t[i].notes * sizeof(struct Note));

    if (i>=8)
    { t[i].sample += 100;  // drum
      t[i].noLoop = 1;
      t[i].drum = 1;
    }
    else
    { t[i].drum = 0;
    }

    for (j=0; j<t[i].notes; ++j) read(&note[i][j].start, 4);
    for (j=0; j<t[i].notes; ++j) read(&note[i][j].key, 1);
    for (j=0; j<t[i].notes; ++j) read(&note[i][j].len, 1);
    for (j=0; j<t[i].notes; ++j) read(&note[i][j].vol, 1);
    for (j=0; j<t[i].notes; ++j) read(&note[i][j].pan, 1);

    for (j=0; j<t[i].notes; ++j)  // find last beat
      if (rows < note[i][j].start + note[i][j].len + 1)
        rows = note[i][j].start + note[i][j].len + 1;
  }

  barLen = header.measuresPerBar*header.beatsPerMeasure;

  if (header.loopStart < rows)
  { loop = 1;
    bars = header.loopEnd / barLen;  // loop: end the song right afer loopEnd
  }
  else
  { loop = 0;
    bars = (rows+barLen-1) / barLen;  // no loop: finish last bar
  }
  rows = bars*barLen;

  fclose(f);


 // Convert notes to tracks, find number of instruments and tracks

  for (i=tracks=0; i<16; ++i) if (t[i].notes)
  { n[tracks] = malloc(rows * sizeof(struct Track));
    memset(n[tracks], 0, rows * sizeof(struct Track));

    for (j=0; j<t[i].notes; ++j)
    { k = note[i][j].start; if (k >= rows) continue;

      int vol = note[i][j].vol,
          pan = note[i][j].pan;

      vol = (vol==0xff ? VOL : 1+vol/4);
      pan = (pan==0xff ? 0 : (pan-6)*127/6);

     // "new note" or "change note parameters"?
      if (note[i][j].key != 0xff)
      {
        //printf("%u\n", k);
        n[tracks][k].freq = t[i].freqShift - 1000;
        if (t[i].drum)
          n[tracks][k].freq += 800 * note[i][j].key + 100;
        else
          n[tracks][k].freq += 8363 * pow(2, note[i][j].key/12.);

       // non-looping instruments don't need "note off" commands
        if (t[i].noLoop) note[i][j].len = 16;

       // fill rows with note parameters
        do
        { n[tracks][k].vol = vol;
          n[tracks][k].pan = pan;
        } while (--note[i][j].len && ++k<rows && !(n[tracks][k].freq));
      }
      else
      { for ( ; n[tracks][k].vol && !(n[tracks][k].freq) && k<rows; ++k)
        { if (note[i][j].vol != 0xff) n[tracks][k].vol = vol;
          if (note[i][j].pan != 0xff) n[tracks][k].pan = pan;
        }
      }

      t[tracks] = t[i];  // squish instrument info
    }
    ++tracks;
    free(note[i]);
  }


 // Find the best bmp+tempo combination, bpm preferably around 125

  unsigned bestTempo=0, bestBPM=0, bestE=-1;

  for (i=1; i<32; ++i)
  { unsigned bpm = 2500*i/header.msPerBeat;
    if (bpm>31 && bpm<256)
    { int e = abs(2500000*i/bpm - header.msPerBeat*1000);
      if (bestE>e || (bestE==e && abs(bestBPM-125)>abs(bpm-125)))
      { bestTempo = i;
        bestBPM = bpm;
        bestE = e;
      }
    }
  }

  if (!bestTempo) goto Err5;


 // Find best finetune: minimize frequency distortion and E5x usage

  for (i=0; i<tracks; ++i)
  { double bestE=1e30; U8 bestFinetune;

    for (k=-64; k<64; k++)  // try every possible finetune
    { double e = 0;

      t[i].finetune = k;

     // mimic pattern encoding to find whether finetune is available
      for (j=0; j<rows; j++)
      { encode(i, j);
        if (wKey && key!=0x60)
        { if (!wFine) finetune = 0;  // can't use finetune on this note :-(

          float logfreq = log2(8363) + (key + finetune/8. + t[i].finetune/128. - t[i].drum*36)/12.;
          float d = log2(n[i][j].freq) - logfreq;
          e += d*d + (finetune ? 1e-8 : 0);
        }
        if (e > bestE) goto nextk;  // break if already worse than the best
      }
      bestFinetune = k;
      bestE = e;

     nextk: continue;

    }
    t[i].finetune = bestFinetune;
  }


 // Join instruments with the same sample, loop type and finetune

  for (i=0; i<tracks; ++i) t[i].instrument = i;

  for (i=0; i<tracks; ++i) if (t[i].instrument == i)
  { for (j=i+1; j<tracks; ++j)
    { if (t[j].sample == t[i].sample && t[j].finetune == t[i].finetune &&
          t[j].noLoop == t[i].noLoop)
        t[j].instrument = i;
    }
  }

  for (instruments=0, i=0; i<tracks; ++i)  // renumber them sequentially
  { if (t[i].instrument == i) t[i].instrument = ++instruments;
    else t[i].instrument = t[t[i].instrument].instrument;
  }


 // Create XM patterns

  for (k=0; k<bars; ++k)
  { U8 *buf = pat[k] = malloc(5*barLen*tracks+9);
    memset(buf, 0, 5*barLen*tracks+9);

    *(U32*)&buf[0] = len = 9;
    *(U16*)&buf[5] = barLen;

    for (j=k*barLen; j<(k+1)*barLen; ++j) for (i=0; i<tracks; ++i)
    { encode(i, j);

      U8 p = 0x80 | wKey | wInst*2 | (wVol||wPanVol)*4 | (wPan||wSkip||wFine)*24;
      if (p != 0x9F) buf[len++] = p;

     // key column
      if (wKey) buf[len++] = key+1;

     // instrument column
      if (wInst) buf[len++] = t[i].instrument;

     // volume column
      if (wVol) buf[len++] = 0x10 + n[i][j].vol;
      else if (wPanVol)
        buf[len++] = 0xC0 + (n[i][j].pan>0x77 ? 0xF : n[i][j].pan+0x88>>4);

     // effect column
      if (wSkip) { buf[len++] = 0xB; buf[len++] = header.loopStart / barLen; }
      else if (wPan) { buf[len++] = 8; buf[len++] = n[i][j].pan + 0x80; }
      else if (wFine) { buf[len++] = 0xE; buf[len++] = 0x58 + finetune; }
    }

    *(U16*)&buf[7] = len-9;
    patLen[k] = len;
  }


 // Find duplicate patterns

  for (i=0; i<bars; ++i) patTable[i] = i;

  for (i=0; i<bars; ++i) if (patTable[i] == i)
    for (j=i+1; j<bars; ++j)
      if (patLen[i] == patLen[j] && !memcmp(pat[i], pat[j], patLen[i]))
        patTable[j] = i;

  for (patterns=0, i=0; i<bars; ++i)  // renumber them sequentially
  { if (patTable[i] == i) patTable[i] = patterns++;
    else patTable[i] = patTable[patTable[i]];
  }


 // Save XM header and patterns

 // "path/Name.org" -> "path/Name.xm"
  argv[1][strlen(argv[1])-3] = 'x';
  argv[1][strlen(argv[1])-2] = 'm';
  argv[1][strlen(argv[1])-1] = 0;
  if (!(g = fopen(argv[1], "wb"))) goto Err2;

 // "path/Name.xm" -> "Name"
  for (i=strlen(argv[1]); i>0 && argv[1][i-1]!='\\' && argv[1][i-1]!='/'; --i) ;

  argv[1][strlen(argv[1])-3] = 0;
  memcpy(xmh.moduleName, &argv[1][i], strlen(&argv[1][i])>20 ? 20 : strlen(&argv[1][i]));
  xmh.songLength = bars;
  xmh.restartPosition = header.loopStart / barLen;
  xmh.channels = tracks;
  xmh.patterns = patterns;
  xmh.instruments = instruments;
  xmh.flags = 1;
  xmh.tempo = bestTempo;
  xmh.bpm = bestBPM;
  memcpy(xmh.patternOrder, patTable, bars);
  write(&xmh, sizeof(struct XMHeader));

  for (k=0, i=0; i<bars; ++i) if (patTable[i] == k)
  { write(pat[i], patLen[i]);
    ++k;
  }


 // Save XM instruments and samples

  for (k=1, i=0; i<tracks; ++i) if (t[i].instrument == k)
  { int len;

    sprintf(smp.sampleName, "samples/%03d.wav", t[i].sample);
    if (!(f = fopen(smp.sampleName, "rb"))) goto Err4;
    read(buf, 44);
    int smpBytes = *(U32*)&buf[0x1C] / *(U32*)&buf[0x18];  // 1=8bit 2=16bit
    len = read(buf, 65536);  // read whole file
    smp.sampleLength = len;

    smp.type = (smpBytes==1 ? 0 : 0x10) | !t[i].noLoop;
    smp.loopStart = 0;
    smp.loopLength = (t[i].noLoop ? 0 : len);
    smp.finetune = t[i].finetune;

    memset(smp.instrumentName, 0, 22);
    if (t[i].drum)
    { strcpy(smp.instrumentName, drumName[t[i].sample-100]);
      smp.relativeKey = 12;

      for (j=len-1; j>0; --j)  // delta encoding
        buf[j] -= buf[j-1];
      buf[0] ^= 0x80;
    }
    else
    { sprintf(smp.instrumentName,
       t[i].freqShift==1000 ? "Melody%02d" : "Melody%02d %+d Hz",
       t[i].sample, t[i].freqShift-1000);
      smp.relativeKey = 36;

      S16 *b = (S16*)buf;
      for (j=smp.sampleLength-1; j>0; --j)  // delta encoding
        b[j] -= b[j-1];
    }

    fclose(f);

    write(&smp, sizeof(struct XMInstrument));
    write(buf, len);

    ++k;
  }


 // Cleanup, error messages

  fclose(g);
  for (i=0; i<tracks; ++i) { free(n[i]); }
  for (k=0; k<bars; ++k) { free(pat[k]); }
  return 0;

Err5: printf("Speed out of XM range!\n"); return 5;
Err4: printf("Couldn't open sample \"%s\"!\n", smp.sampleName); return 4;
Err3: printf("Invalid header!\n"); return 3;
Err2: printf("Couldn't open file \"%s\"!\n", argv[1]); return 2;
Err1: printf("Usage: \"org2xm infile.org\"\n"
             "    OR \"org2xm infile.org c\" (for compatible output)"); return 1;
}
