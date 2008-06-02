/*
** Haaf's Game Engine 1.7
** Copyright (C) 2003-2007, Relish Games
** hge.relishgames.com
**
** hgeDistortionMesh helper class header
*/


#ifndef HGEDISTORT_H
#define HGEDISTORT_H


#include "hge.h"


#define HGEDISP_NODE		0
#define HGEDISP_TOPLEFT		1
#define HGEDISP_CENTER		2

/*
** HGE Distortion mesh class
*/
class hgeDistortionMesh
{
public:
     hgeDistortionMesh(int cols, int rows);
     hgeDistortionMesh(const hgeDistortionMesh &dm);
     ~hgeDistortionMesh();

	 hgeDistortionMesh&	operator= (const hgeDistortionMesh &dm);

     virtual void		CALL	Render(float x, float y); // 1
     virtual void		CALL	Clear(DWORD col=0xFFFFFFFF, float z=0.5f); // 2

     virtual void		CALL	SetTexture(HTEXTURE tex); // 3
     virtual void		CALL	SetTextureRect(float x, float y, float w, float h); // 4
     virtual void		CALL	SetBlendMode(int blend); // 5
     virtual void		CALL	SetZ(int col, int row, float z); // 6
     virtual void		CALL	SetColor(int col, int row, DWORD color); // 7
     virtual void		CALL	SetDisplacement(int col, int row, float dx, float dy, int ref); // 8

     virtual HTEXTURE	CALL	GetTexture() const {return quad.tex;} // 9
     virtual void		CALL	GetTextureRect(float *x, float *y, float *w, float *h) const { *x=tx; *y=ty; *w=width; *h=height; } // 10
     virtual int		CALL	GetBlendMode() const { return quad.blend; } // 11
     virtual float		CALL	GetZ(int col, int row) const; // 12
     virtual DWORD		CALL	GetColor(int col, int row) const; // 13
     virtual void		CALL	GetDisplacement(int col, int row, float *dx, float *dy, int ref) const; // 14

	 virtual int		CALL	GetRows() { return nRows; } // 15
	 virtual int		CALL	GetCols() { return nCols; } // 16

private:
	hgeDistortionMesh();

	static HGE	*hge;

	hgeVertex	*disp_array;
	int			nRows, nCols;
	float		cellw,cellh;
	float		tx,ty,width,height;
	hgeQuad		quad;
};


#endif
