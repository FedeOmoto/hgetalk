/*
** Haaf's Game Engine 1.7
** Copyright (C) 2003-2007, Relish Games
** hge.relishgames.com
**
** hgeParticleSystem helper class header
*/


#ifndef HGEPARTICLE_H
#define HGEPARTICLE_H


#include "hge.h"
#include "hgesprite.h"
#include "hgevector.h"
#include "hgecolor.h"
#include "hgerect.h"


#define MAX_PARTICLES	500
#define MAX_PSYSTEMS	100

struct hgeParticle
{
	hgeVector	vecLocation;
	hgeVector	vecVelocity;

	float		fGravity;
	float		fRadialAccel;
	float		fTangentialAccel;

	float		fSpin;
	float		fSpinDelta;

	float		fSize;
	float		fSizeDelta;

	hgeColor	colColor;		// + alpha
	hgeColor	colColorDelta;

	float		fAge;
	float		fTerminalAge;
};

struct hgeParticleSystemInfo
{
	hgeSprite*	sprite;    // texture + blend mode
	int			nEmission; // particles per sec
	float		fLifetime;

	float		fParticleLifeMin;
	float		fParticleLifeMax;

	float		fDirection;
	float		fSpread;
	bool		bRelative;

	float		fSpeedMin;
	float		fSpeedMax;

	float		fGravityMin;
	float		fGravityMax;

	float		fRadialAccelMin;
	float		fRadialAccelMax;

	float		fTangentialAccelMin;
	float		fTangentialAccelMax;

	float		fSizeStart;
	float		fSizeEnd;
	float		fSizeVar;

	float		fSpinStart;
	float		fSpinEnd;
	float		fSpinVar;

	hgeColor	colColorStart; // + alpha
	hgeColor	colColorEnd;
	float		fColorVar;
	float		fAlphaVar;
};

class hgeParticleSystem
{
public:
	hgeParticleSystemInfo info;
	
	hgeParticleSystem(const char *filename, hgeSprite *sprite);
	hgeParticleSystem(hgeParticleSystemInfo *psi);
	hgeParticleSystem(const hgeParticleSystem &ps);
	~hgeParticleSystem() { hge->Release(); }

	hgeParticleSystem&	operator= (const hgeParticleSystem &ps);


	virtual void		CALL	Render(); // 1
	virtual void		CALL	FireAt(float x, float y); // 2
	virtual void		CALL	Fire(); // 3
	virtual void		CALL	Stop(bool bKillParticles=false); // 4
	virtual void		CALL	Update(float fDeltaTime); // 5
	virtual void		CALL	MoveTo(float x, float y, bool bMoveParticles=false); // 6
	virtual void		CALL	Transpose(float x, float y) { fTx=x; fTy=y; } // 7
	virtual void		CALL	SetScale(float scale) { fScale = scale; } // 8
	virtual void		CALL	TrackBoundingBox(bool bTrack) { bUpdateBoundingBox=bTrack; } // 9

	virtual int			CALL	GetParticlesAlive() const { return nParticlesAlive; } // 10
	virtual float		CALL	GetAge() const { return fAge; } // 11
	virtual void		CALL	GetPosition(float *x, float *y) const { *x=vecLocation.x; *y=vecLocation.y; } // 12
	virtual void		CALL	GetTransposition(float *x, float *y) const { *x=fTx; *y=fTy; } // 13
	virtual float		CALL	GetScale() { return fScale; } // 14
	virtual hgeRect*	CALL	GetBoundingBox(hgeRect *rect) const; // 15

private:
	hgeParticleSystem();

	static HGE			*hge;

	float				fAge;
	float				fEmissionResidue;

	hgeVector			vecPrevLocation;
	hgeVector			vecLocation;
	float				fTx, fTy;
	float				fScale;

	int					nParticlesAlive;
	hgeRect				rectBoundingBox;
	bool				bUpdateBoundingBox;

	hgeParticle			particles[MAX_PARTICLES];
};

class hgeParticleManager
{
public:
	hgeParticleManager();
	~hgeParticleManager();

	void				Update(float dt);
	void				Render();

	hgeParticleSystem*	SpawnPS(hgeParticleSystemInfo *psi, float x, float y);
	bool				IsPSAlive(hgeParticleSystem *ps) const;
	void				Transpose(float x, float y);
	void				GetTransposition(float *dx, float *dy) const {*dx=tX; *dy=tY;}
	void				KillPS(hgeParticleSystem *ps);
	void				KillAll();

private:
	hgeParticleManager(const hgeParticleManager &);
	hgeParticleManager&	operator= (const hgeParticleManager &);

	int					nPS;
	float				tX;
	float				tY;
	hgeParticleSystem*	psList[MAX_PSYSTEMS];
};


#endif
