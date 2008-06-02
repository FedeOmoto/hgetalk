/*
** Haaf's Game Engine 1.7
** Copyright (C) 2003-2007, Relish Games
** hge.relishgames.com
**
** hgeSprite helper class header
*/


#ifndef HGESPRITE_H
#define HGESPRITE_H


#include "hge.h"
#include "hgerect.h"


/*
** HGE Sprite class
*/
class hgeSprite
{
public:
	hgeSprite(HTEXTURE tex, float x, float y, float w, float h);
	hgeSprite(const hgeSprite &spr);
	~hgeSprite() { hge->Release(); }
	
	
	virtual void		CALL	Render(float x, float y); // 1
	virtual void		CALL	RenderEx(float x, float y, float rot, float hscale=1.0f, float vscale=0.0f); // 2
	virtual void		CALL	RenderStretch(float x1, float y1, float x2, float y2); // 3
	virtual void		CALL	Render4V(float x0, float y0, float x1, float y1, float x2, float y2, float x3, float y3); // 4

	virtual void		CALL	SetTexture(HTEXTURE tex); // 5
	virtual void		CALL	SetTextureRect(float x, float y, float w, float h, bool adjSize = true); // 6
	virtual void		CALL	SetColor(DWORD col, int i=-1); // 7
	virtual void		CALL	SetZ(float z, int i=-1); // 8
	virtual void		CALL	SetBlendMode(int blend) { quad.blend=blend; } // 9
	virtual void		CALL	SetHotSpot(float x, float y) { hotX=x; hotY=y; } // 10
	virtual void		CALL	SetFlip(bool bX, bool bY, bool bHotSpot = false); // 11

	virtual HTEXTURE	CALL	GetTexture() const { return quad.tex; } // 12
	virtual void		CALL	GetTextureRect(float *x, float *y, float *w, float *h) const { *x=tx; *y=ty; *w=width; *h=height; } // 13
	virtual DWORD		CALL	GetColor(int i=0) const { return quad.v[i].col; } // 14
	virtual float		CALL	GetZ(int i=0) const { return quad.v[i].z; } // 15
	virtual int			CALL	GetBlendMode() const { return quad.blend; } // 16
	virtual void		CALL	GetHotSpot(float *x, float *y) const { *x=hotX; *y=hotY; } // 17
	virtual void		CALL	GetFlip(bool *bX, bool *bY) const { *bX=bXFlip; *bY=bYFlip; } // 18

	virtual float		CALL	GetWidth() const { return width; } // 19
	virtual float		CALL	GetHeight() const { return height; } // 20
	virtual hgeRect*	CALL	GetBoundingBox(float x, float y, hgeRect *rect) const { rect->Set(x-hotX, y-hotY, x-hotX+width, y-hotY+height); return rect; } // 21
	virtual hgeRect*	CALL	GetBoundingBoxEx(float x, float y, float rot, float hscale, float vscale,  hgeRect *rect) const; // 22

protected:
	hgeSprite();
	static HGE	*hge;

	hgeQuad		quad;
	float		tx, ty, width, height;
	float		tex_width, tex_height;
	float		hotX, hotY;
	bool		bXFlip, bYFlip, bHSFlip;
};


#endif
