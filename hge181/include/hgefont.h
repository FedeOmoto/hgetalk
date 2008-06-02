/*
** Haaf's Game Engine 1.7
** Copyright (C) 2003-2007, Relish Games
** hge.relishgames.com
**
** hgeFont helper class header
*/


#ifndef HGEFONT_H
#define HGEFONT_H


#include "hge.h"
#include "hgesprite.h"


#define HGETEXT_LEFT		0
#define HGETEXT_RIGHT		1
#define HGETEXT_CENTER		2
#define HGETEXT_HORZMASK	0x03

#define HGETEXT_TOP			0
#define HGETEXT_BOTTOM		4
#define HGETEXT_MIDDLE		8
#define HGETEXT_VERTMASK	0x0C

/*
** HGE Font class
*/
class hgeFont
{
public:
	hgeFont(const char *filename, bool bMipmap=false);
	~hgeFont();

	virtual	void		CALL	Render(float x, float y, int align, const char *string); // 1
	virtual void		CALL	printf(float x, float y, int align, const char *format, ...); // 2
	virtual void		CALL	printfb(float x, float y, float w, float h, int align, const char *format, ...); // 3

	virtual void		CALL	SetColor(DWORD col); // 4
	virtual void		CALL	SetZ(float z); // 5
	virtual void		CALL	SetBlendMode(int blend); // 6
	virtual void		CALL	SetScale(float scale) {fScale=scale;} // 7
	virtual void		CALL	SetProportion(float prop) { fProportion=prop; } // 8
	virtual void		CALL	SetRotation(float rot) {fRot=rot;} // 9
	virtual void		CALL	SetTracking(float tracking) {fTracking=tracking;} // 10
	virtual void		CALL	SetSpacing(float spacing) {fSpacing=spacing;} // 11

	virtual DWORD		CALL	GetColor() const {return dwCol;} // 12
	virtual float		CALL	GetZ() const {return fZ;} // 13
	virtual int			CALL	GetBlendMode() const {return nBlend;} // 14
	virtual float		CALL	GetScale() const {return fScale;} // 15
	virtual float		CALL	GetProportion() const { return fProportion; } // 16
	virtual float		CALL	GetRotation() const {return fRot;} // 17
	virtual float		CALL	GetTracking() const {return fTracking;} // 18
	virtual float		CALL	GetSpacing() const {return fSpacing;} // 19

	virtual hgeSprite*	CALL	GetSprite(char chr) const { return letters[(unsigned char)chr]; } // 20
	virtual float		CALL	GetPreWidth(char chr) const { return pre[(unsigned char)chr]; } // 21
	virtual float		CALL	GetPostWidth(char chr) const { return post[(unsigned char)chr]; } // 22
	virtual float		CALL	GetHeight() const { return fHeight; } // 23
	virtual float		CALL	GetStringWidth(const char *string, bool bMultiline=true) const; // 24

private:
	hgeFont();
	hgeFont(const hgeFont &fnt);
	hgeFont&	operator= (const hgeFont &fnt);

	char*		_get_line(char *file, char *line);

	static HGE	*hge;

	static char	buffer[1024];

	HTEXTURE	hTexture;
	hgeSprite*	letters[256];
	float		pre[256];
	float		post[256];
	float		fHeight;
	float		fScale;
	float		fProportion;
	float		fRot;
	float		fTracking;
	float		fSpacing;

	DWORD		dwCol;
	float		fZ;
	int			nBlend;
};


#endif
